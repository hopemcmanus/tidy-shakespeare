library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(igraph)
library(visNetwork)
library(text2map)
library(purrr)

# Load metadata
data(meta_shakespeare)

# Load tokenized data
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

# Create output directory
interactive_dir <- "interactive_networks/characters"
if(!dir.exists(interactive_dir)) dir.create(interactive_dir, recursive = TRUE)

# Build character network function - NO PRE-FILTERING
build_character_network <- function(play_data, min_cooccurrence = 1) {
  
  scene_data <- play_data %>%
    filter(!is.na(character), character != "") %>%
    filter(!str_detect(tolower(character), "^(all|both)")) %>%
    mutate(character = str_split(character, ",")) %>%
    unnest(character) %>%
    mutate(character = str_split(character, regex("\\s+and\\s+", ignore_case = TRUE))) %>%
    unnest(character) %>%
    mutate(character = str_trim(character)) %>%
    filter(character != "", !is.na(character))
  
  if(nrow(scene_data) == 0) {
    return(graph.empty(n = 0, directed = FALSE))
  }
  
  char_stats <- scene_data %>%
    group_by(character) %>%
    summarise(
      n_tokens = n(),
      n_scenes = n_distinct(scene),
      .groups = "drop"
    )
  
  scene_character <- scene_data %>%
    group_by(scene, character) %>%
    summarise(present = 1, .groups = "drop") %>%
    pivot_wider(names_from = character, values_from = present, values_fill = 0)
  
  if(ncol(scene_character) <= 1) {
    return(graph.empty(n = 0, directed = FALSE))
  }
  
  char_matrix <- as.matrix(scene_character[, -1])
  
  cooccur_matrix <- t(char_matrix) %*% char_matrix
  diag(cooccur_matrix) <- 0
  
  g <- graph_from_adjacency_matrix(
    cooccur_matrix, 
    mode = "undirected", 
    weighted = TRUE
  )
  
  V(g)$n_tokens <- char_stats$n_tokens[match(V(g)$name, char_stats$character)]
  V(g)$n_scenes <- char_stats$n_scenes[match(V(g)$name, char_stats$character)]
  
  if(min_cooccurrence > 1) {
    g <- delete_edges(g, E(g)[weight < min_cooccurrence])
  }
  
  g <- delete_vertices(g, V(g)[degree(g) == 0])
  
  if(vcount(g) > 0) {
    V(g)$degree <- degree(g)
    V(g)$betweenness <- betweenness(g)
    V(g)$closeness <- closeness(g)
    
    communities <- tryCatch(
      cluster_louvain(g),
      error = function(e) NULL
    )
    
    if(!is.null(communities)) {
      V(g)$community <- membership(communities)
    } else {
      V(g)$community <- 1
    }
  }
  
  return(g)
}

# Create interactive visualization function
create_interactive_network <- function(g, play_title) {
  
  if(vcount(g) == 0) {
    return(NULL)
  }
  
  nodes <- data.frame(
    id = V(g)$name,
    label = V(g)$name,
    title = paste0(
      "<b>", V(g)$name, "</b><br>",
      "Tokens: ", V(g)$n_tokens, "<br>",
      "Scenes: ", V(g)$n_scenes, "<br>",
      "Degree: ", V(g)$degree, "<br>",
      "Betweenness: ", round(V(g)$betweenness, 2)
    ),
    value = V(g)$degree,
    group = V(g)$community,
    stringsAsFactors = FALSE
  )
  
  edges_df <- as_data_frame(g, what = "edges")
  edges <- data.frame(
    from = edges_df$from,
    to = edges_df$to,
    value = edges_df$weight,
    title = paste0("Co-occurrences: ", edges_df$weight),
    stringsAsFactors = FALSE
  )
  
  network <- visNetwork(nodes, edges, 
                        main = list(text = play_title, style = "font-size:20px;font-weight:bold;")) %>%
    visIgraphLayout(layout = "layout_with_fr") %>%
    visNodes(
      shape = "dot",
      scaling = list(min = 10, max = 40),
      font = list(size = 14)
    ) %>%
    visEdges(
      scaling = list(min = 1, max = 10),
      smooth = list(enabled = TRUE, type = "continuous")
    ) %>%
    visOptions(
      highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
      selectedBy = list(variable = "group", main = "Select by community"),
      nodesIdSelection = list(enabled = TRUE, main = "Select character")
    ) %>%
    visInteraction(
      navigationButtons = TRUE,
      hover = TRUE,
      tooltipDelay = 100
    ) %>%
    visPhysics(
      stabilization = TRUE,
      barnesHut = list(gravitationalConstant = -2000, springLength = 100)
    ) %>%
    visLegend(
      width = 0.1,
      position = "right",
      main = "Community"
    )
  
  return(network)
}

# Get list of unique plays
plays <- meta_shakespeare %>%
  select(gutenberg_id, short_title, genre, year) %>%
  arrange(short_title)

cat("\n", strrep("=", 60), "\n")
cat("GENERATING INTERACTIVE CHARACTER NETWORKS\n")
cat(strrep("=", 60), "\n\n")

successful_saves <- 0

for(i in seq_len(nrow(plays))) {
  
  play_id <- plays$gutenberg_id[i]
  play_title <- plays$short_title[i]
  
  cat("Processing:", play_title, "(", play_id, ")\n")
  
  play_data <- tokens %>%
    filter(gutenberg_id == play_id | short_title == play_title)
  
  if(nrow(play_data) == 0) {
    cat("  ⚠ No data found\n\n")
    next
  }
  
  g <- build_character_network(play_data, min_cooccurrence = 2)
  
  if(vcount(g) == 0) {
    cat("  ⚠ No network (insufficient co-occurrences)\n\n")
    next
  }
  
  cat("  Nodes:", vcount(g), "| Edges:", ecount(g), "\n")
  
  network <- create_interactive_network(g, play_title)
  
  if(!is.null(network)) {
    filename <- str_to_lower(play_title) %>%
      str_replace_all("[^a-z0-9]+", "_") %>%
      str_replace_all("^_+|_+$", "")
    
    visSave(
      network,
      file = file.path(interactive_dir, paste0(filename, ".html"))
    )
    
    successful_saves <- successful_saves + 1
    cat("  ✓ Saved interactive network\n\n")
  }
}

# Create index HTML
index_html <- c(
  "<!DOCTYPE html>",
  "<html>",
  "<head>",
  "  <title>Shakespeare Character Networks</title>",
  "  <style>",
  "    body { font-family: Arial, sans-serif; max-width: 1200px; margin: 50px auto; padding: 20px; }",
  "    h1 { color: #2c3e50; border-bottom: 3px solid #3498db; padding-bottom: 10px; }",
  "    .play-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(300px, 1fr)); gap: 20px; }",
  "    .play-card { border: 1px solid #ddd; border-radius: 8px; padding: 15px; background: #f9f9f9; }",
  "    .play-card:hover { box-shadow: 0 4px 8px rgba(0,0,0,0.1); }",
  "    .play-title { font-size: 18px; font-weight: bold; margin-bottom: 10px; color: #2c3e50; }",
  "    .play-meta { font-size: 14px; color: #7f8c8d; margin-bottom: 10px; }",
  "    .play-link { display: inline-block; padding: 8px 16px; background: #3498db; color: white; ",
  "                 text-decoration: none; border-radius: 4px; margin-top: 10px; }",
  "    .play-link:hover { background: #2980b9; }",
  "  </style>",
  "</head>",
  "<body>",
  "  <h1>Shakespeare Character Co-occurrence Networks</h1>",
  "  <p>Interactive network visualizations showing character relationships in Shakespeare's plays.</p>",
  "  <p>Network edges represent scene co-occurrences between characters. Node size indicates degree centrality.</p>",
  "  <div class='play-grid'>"
)

for(i in seq_len(nrow(plays))) {
  play_id <- plays$gutenberg_id[i]
  play_title <- plays$short_title[i]
  genre <- plays$genre[i]
  year <- plays$year[i]
  
  filename <- str_to_lower(play_title) %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_replace_all("^_+|_+$", "")
  
  html_file <- paste0(filename, ".html")
  
  if(file.exists(file.path(interactive_dir, html_file))) {
    index_html <- c(
      index_html,
      paste0("    <div class='play-card'>"),
      paste0("      <div class='play-title'>", play_title, "</div>"),
      paste0("      <div class='play-meta'>Genre: ", genre, " | Year: ", year, "</div>"),
      paste0("      <a class='play-link' href='", html_file, "'>View Network →</a>"),
      paste0("    </div>")
    )
  }
}

index_html <- c(
  index_html,
  "  </div>",
  "</body>",
  "</html>"
)

writeLines(index_html, file.path(interactive_dir, "index.html"))

cat("\n", strrep("=", 60), "\n")
cat("SUMMARY\n")
cat(strrep("=", 60), "\n\n")

cat("Total plays processed:", nrow(plays), "\n")
cat("Interactive networks created:", successful_saves, "\n")
cat("\n✓ Complete!\n")
cat("\nOutput location:", interactive_dir, "\n")
cat("Open 'index.html' in a web browser to explore all networks\n")