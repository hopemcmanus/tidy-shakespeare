library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(igraph)
library(ggraph)
library(ggplot2)
library(text2map)
library(purrr)

# Load metadata
data(meta_shakespeare)

# Load tokenized data - handles both individual files and combined file
tokens_dir <- "data/processed/tokens/"

if(file.exists(file.path(tokens_dir, "all_shakespeare_tokens.csv"))) {
  cat("Loading combined tokenized file...\n")
  tokens <- read_csv(file.path(tokens_dir, "all_shakespeare_tokens.csv"), show_col_types = FALSE)
} else {
  cat("Loading individual play token files...\n")
  token_files <- list.files(tokens_dir, pattern = "_tokens.csv$", full.names = TRUE)
  token_files <- token_files[!grepl("all_shakespeare_tokens.csv", token_files)]
  tokens <- map_dfr(token_files, ~read_csv(.x, show_col_types = FALSE))
}

# Create output directories
plot_dir <- "plots/character_networks"
if(!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)

network_dir <- "data/networks"
if(!dir.exists(network_dir)) dir.create(network_dir, recursive = TRUE)

# Build character network function - NO PRE-FILTERING
build_character_network <- function(play_data, min_cooccurrence = 1) {
  
  # Process ALL characters in the data without pre-filtering
  scene_data <- play_data %>%
    filter(!is.na(character), character != "") %>%
    # Only filter out obvious collective entries
    filter(!str_detect(tolower(character), "^(all|both)")) %>%
    # Split compound names
    mutate(character = str_split(character, ",")) %>%
    unnest(character) %>%
    mutate(character = str_split(character, regex("\\s+and\\s+", ignore_case = TRUE))) %>%
    unnest(character) %>%
    mutate(character = str_trim(character)) %>%
    filter(character != "", !is.na(character))
  
  if(nrow(scene_data) == 0) {
    return(graph.empty(n = 0, directed = FALSE))
  }
  
  # Create scene × character matrix
  scene_character <- scene_data %>%
    group_by(scene, character) %>%
    summarise(present = 1, .groups = "drop") %>%
    pivot_wider(names_from = character, values_from = present, values_fill = 0)
  
  if(ncol(scene_character) <= 1) {
    return(graph.empty(n = 0, directed = FALSE))
  }
  
  char_matrix <- as.matrix(scene_character[, -1])
  
  # Calculate co-occurrence matrix
  cooccur_matrix <- t(char_matrix) %*% char_matrix
  diag(cooccur_matrix) <- 0
  
  # Create igraph object
  g <- graph_from_adjacency_matrix(
    cooccur_matrix, 
    mode = "undirected", 
    weighted = TRUE
  )
  
  # Filter weak edges only (keep all nodes initially)
  if(min_cooccurrence > 1) {
    g <- delete_edges(g, E(g)[weight < min_cooccurrence])
  }
  
  # Remove only isolated vertices (degree = 0)
  g <- delete_vertices(g, V(g)[degree(g) == 0])
  
  return(g)
}

# Calculate network metrics function
calculate_network_metrics <- function(g, play_title, gutenberg_id) {
  
  if(vcount(g) == 0) {
    return(tibble(
      gutenberg_id = gutenberg_id,
      short_title = play_title,
      n_nodes = 0,
      n_edges = 0,
      density = NA,
      avg_degree = NA,
      diameter = NA,
      transitivity = NA,
      modularity = NA
    ))
  }
  
  n_nodes <- vcount(g)
  n_edges <- ecount(g)
  dens <- edge_density(g)
  avg_deg <- mean(degree(g))
  
  diam <- tryCatch(diameter(g), error = function(e) NA)
  trans <- transitivity(g, type = "global")
  
  communities <- tryCatch(
    cluster_louvain(g),
    error = function(e) NULL
  )
  
  modul <- if(!is.null(communities)) modularity(communities) else NA
  
  tibble(
    gutenberg_id = gutenberg_id,
    short_title = play_title,
    n_nodes = n_nodes,
    n_edges = n_edges,
    density = dens,
    avg_degree = avg_deg,
    diameter = diam,
    transitivity = trans,
    modularity = modul
  )
}

# Plot character network function
plot_character_network <- function(g, play_title, layout = "fr") {
  
  if(vcount(g) == 0) {
    return(NULL)
  }
  
  deg <- degree(g)
  
  communities <- tryCatch(
    cluster_louvain(g),
    error = function(e) NULL
  )
  
  p <- ggraph(g, layout = layout) +
    geom_edge_link(
      aes(width = weight, alpha = weight),
      colour = "gray50"
    ) +
    scale_edge_width(range = c(0.3, 2)) +
    scale_edge_alpha(range = c(0.3, 0.8)) +
    geom_node_point(
      aes(size = deg, colour = if(!is.null(communities)) factor(membership(communities)) else "1"),
      alpha = 0.8
    ) +
    scale_size(range = c(3, 12), name = "Degree") +
    scale_colour_brewer(palette = "Set3", guide = "none") +
    geom_node_text(
      aes(label = name),
      repel = TRUE,
      max.overlaps = 30,
      size = 2.5,
      fontface = "bold"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 10),
      legend.position = "bottom"
    ) +
    labs(
      title = play_title,
      subtitle = paste0(
        "Nodes: ", vcount(g), " | ",
        "Edges: ", ecount(g), " | ",
        "Density: ", round(edge_density(g), 3)
      )
    )
  
  return(p)
}

# Get list of unique plays
plays <- meta_shakespeare %>%
  select(gutenberg_id, short_title, genre, year) %>%
  arrange(short_title)

# Storage for network metrics and objects
all_metrics <- list()
all_networks <- list()

cat("\n", strrep("=", 60), "\n")
cat("GENERATING CHARACTER CO-OCCURRENCE NETWORKS\n")
cat(strrep("=", 60), "\n\n")

for(i in seq_len(nrow(plays))) {
  
  play_id <- plays$gutenberg_id[i]
  play_title <- plays$short_title[i]
  
  cat("Processing:", play_title, "(", play_id, ")\n")
  
  play_data <- tokens %>%
    filter(gutenberg_id == play_id | short_title == play_title)
  
  if(nrow(play_data) == 0) {
    cat("  ⚠ No data found for this play\n\n")
    next
  }
  
  # Count unique characters before building network
  char_count <- play_data %>%
    filter(!is.na(character), character != "") %>%
    filter(!str_detect(tolower(character), "^(all|both)")) %>%
    distinct(character) %>%
    nrow()
  
  cat("  Raw characters in data:", char_count, "\n")
  
  g <- build_character_network(play_data, min_cooccurrence = 2)
  
  if(vcount(g) == 0) {
    cat("  ⚠ No network (no characters or no co-occurrences)\n\n")
    next
  }
  
  all_networks[[play_title]] <- g
  
  metrics <- calculate_network_metrics(g, play_title, play_id)
  all_metrics[[play_title]] <- metrics
  
  cat("  Nodes in network:", vcount(g), "| Edges:", ecount(g), "\n")
  
  p <- plot_character_network(g, play_title, layout = "fr")
  
  if(!is.null(p)) {
    filename <- str_to_lower(play_title) %>%
      str_replace_all("[^a-z0-9]+", "_") %>%
      str_replace_all("^_+|_+$", "")
    
    ggsave(
      filename = file.path(plot_dir, paste0(filename, "_network.png")),
      plot = p,
      width = 12,
      height = 10,
      dpi = 300
    )
    
    cat("  ✓ Saved plot\n")
  }
  
  write_graph(
    g,
    file = file.path(network_dir, paste0(filename, ".graphml")),
    format = "graphml"
  )
  
  cat("  ✓ Saved network file\n\n")
}

# Combine and save metrics
network_metrics <- bind_rows(all_metrics) %>%
  left_join(plays, by = c("gutenberg_id", "short_title"))

write_csv(network_metrics, file.path(network_dir, "network_metrics.csv"))

cat("✓ Saved network_metrics.csv\n")

cat("\n", strrep("=", 60), "\n")
cat("NETWORK ANALYSIS SUMMARY\n")
cat(strrep("=", 60), "\n\n")

cat("Total plays processed:", nrow(plays), "\n")
cat("Networks created:", length(all_networks), "\n\n")

cat("Network size statistics:\n")
cat("  Average nodes per play:", round(mean(network_metrics$n_nodes, na.rm = TRUE), 1), "\n")
cat("  Average edges per play:", round(mean(network_metrics$n_edges, na.rm = TRUE), 1), "\n")
cat("  Average density:", round(mean(network_metrics$density, na.rm = TRUE), 3), "\n")
cat("  Average degree:", round(mean(network_metrics$avg_degree, na.rm = TRUE), 1), "\n\n")

cat("Top 10 plays by network size (nodes):\n")
print(
  network_metrics %>%
    select(short_title, n_nodes, n_edges, density) %>%
    arrange(desc(n_nodes)) %>%
    head(10),
  n = 10
)

cat("\nTop 10 most dense networks:\n")
print(
  network_metrics %>%
    select(short_title, n_nodes, density, transitivity) %>%
    arrange(desc(density)) %>%
    head(10),
  n = 10
)

cat("\n✓ Complete!\n")
cat("\nOutput locations:\n")
cat("  - Plots:", plot_dir, "\n")
cat("  - Network files:", network_dir, "\n")
cat("  - Metrics:", file.path(network_dir, "network_metrics.csv"), "\n")