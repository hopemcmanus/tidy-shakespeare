library(tidyverse)
library(tidytext)
library(widyr)
library(visNetwork)
library(here)

# Configuration
INPUT_DIR_CLEANED <- here("data", "cleaned")
INPUT_DIR_METADATA <- here("data", "metadata")
OUTPUT_DIR_NETWORKS <- here("interactive_networks", "correlations")

# Filter settings - Customize these to analyze different subsets
# FILTER_BY <- "gutenberg_id"  # Options: "gutenberg_id", "genre", "short_title", "period", "romance", "problem_play", "roman", "all"
# FILTER_VALUES <- c("1516", "1519", "1522", "1524", "1523", "1503", "1540", "1531", "1515")  # HSC plays
# Examples:
# FILTER_BY <- "genre"
# FILTER_VALUES <- c("Tragedy")
# 
# FILTER_BY <- "genre"
# FILTER_VALUES <- c("Comedy")
#
# FILTER_BY <- "short_title"
# FILTER_VALUES <- c("Hamlet", "Othello", "Macbeth")
#
# FILTER_BY <- "period"
# FILTER_VALUES <- c("Elizabethan")  # or "Jacobean"
#
# FILTER_BY <- "romance"
# FILTER_VALUES <- TRUE  # Late romances only
#
# FILTER_BY <- "problem_play"
# FILTER_VALUES <- TRUE  # Problem plays only
#
# FILTER_BY <- "roman"
# FILTER_VALUES <- TRUE  # Roman plays only
#
FILTER_BY <- "all"
FILTER_VALUES <- NULL

# Create output directory
dir.create(OUTPUT_DIR_NETWORKS, recursive = TRUE, showWarnings = FALSE)

message("\n", strrep("=", 70))
message("SHAKESPEARE INTERACTIVE NETWORK VISUALIZATIONS")
message(strrep("=", 70))
message("Input: ", INPUT_DIR_CLEANED)
message("Output: ", OUTPUT_DIR_NETWORKS)
message("Filter: ", FILTER_BY, " = ", paste(FILTER_VALUES, collapse = ", "))
message(strrep("=", 70), "\n")

# Load plays and apply filter
message("Loading plays data...")
all_plays <- read_csv(
  file.path(INPUT_DIR_CLEANED, "all_shakespeare.csv"),
  show_col_types = FALSE
)

# Load metadata for advanced filtering
meta_shakespeare <- read_csv(
  file.path(INPUT_DIR_METADATA, "meta_shakespeare.csv"),
  show_col_types = FALSE
)

# Apply filter based on FILTER_BY setting
if (FILTER_BY == "all") {
  selected_plays <- all_plays %>%
    filter(class == "dialogue")
  message("✓ Loaded ALL plays")
  
} else if (FILTER_BY == "gutenberg_id") {
  selected_plays <- all_plays %>%
    filter(gutenberg_id %in% FILTER_VALUES, class == "dialogue")
  message("✓ Loaded plays by Gutenberg ID")
  
} else if (FILTER_BY == "genre") {
  selected_plays <- all_plays %>%
    filter(genre %in% FILTER_VALUES, class == "dialogue")
  message("✓ Loaded plays by genre: ", paste(FILTER_VALUES, collapse = ", "))
  
} else if (FILTER_BY == "short_title") {
  selected_plays <- all_plays %>%
    filter(short_title %in% FILTER_VALUES, class == "dialogue")
  message("✓ Loaded plays by title: ", paste(FILTER_VALUES, collapse = ", "))
  
} else if (FILTER_BY == "period") {
  # Filter by period (Elizabethan/Jacobean)
  period_ids <- meta_shakespeare %>%
    filter(period %in% FILTER_VALUES) %>%
    pull(gutenberg_id)
  
  selected_plays <- all_plays %>%
    filter(gutenberg_id %in% period_ids, class == "dialogue")
  message("✓ Loaded plays by period: ", paste(FILTER_VALUES, collapse = ", "))
  
} else if (FILTER_BY == "romance") {
  # Filter late romances
  romance_ids <- meta_shakespeare %>%
    filter(romance == FILTER_VALUES) %>%
    pull(gutenberg_id)
  
  selected_plays <- all_plays %>%
    filter(gutenberg_id %in% romance_ids, class == "dialogue")
  message("✓ Loaded late romances")
  
} else if (FILTER_BY == "problem_play") {
  # Filter problem plays
  problem_ids <- meta_shakespeare %>%
    filter(problem_play == FILTER_VALUES) %>%
    pull(gutenberg_id)
  
  selected_plays <- all_plays %>%
    filter(gutenberg_id %in% problem_ids, class == "dialogue")
  message("✓ Loaded problem plays")
  
} else if (FILTER_BY == "roman") {
  # Filter Roman plays
  roman_ids <- meta_shakespeare %>%
    filter(roman == FILTER_VALUES) %>%
    pull(gutenberg_id)
  
  selected_plays <- all_plays %>%
    filter(gutenberg_id %in% roman_ids, class == "dialogue")
  message("✓ Loaded Roman plays")
}

message("  Total dialogue lines: ", format(nrow(selected_plays), big.mark = ","))
message("  Plays included: ", n_distinct(selected_plays$short_title), "\n")

# Prepare section-based word data
message("Creating section-based word correlations...")

shakespeare_section_words <- selected_plays %>%
  arrange(short_title, line_number) %>%
  group_by(short_title) %>%
  mutate(
    line = row_number(),
    section = line %/% 10
  ) %>%
  ungroup() %>%
  filter(section > 0) %>%
  select(short_title, section, text) %>%
  unnest_tokens(word, text) %>%
  # Normalize apostrophes
  mutate(word = str_replace_all(word, "['']", "'")) %>%
  filter(!word %in% stop_words_custom$word) %>%
  count(short_title, section, word, sort = TRUE, name = "n")

message("✓ Prepared word data for ", n_distinct(shakespeare_section_words$short_title), " plays\n")

# Create interactive networks
message("Creating interactive network visualizations...")

titles <- unique(shakespeare_section_words$short_title)
network_count <- 0

for (play in titles) {
  play_data <- shakespeare_section_words %>%
    filter(short_title == play)
  
  # Calculate word correlations
  word_cors <- play_data %>%
    add_count(word, name = "word_total") %>%
    filter(word_total >= 10) %>%
    pairwise_cor(word, section, value = n, sort = TRUE) %>%
    filter(correlation > 0.15)
  
  if (nrow(word_cors) == 0) {
    message("  No edges above threshold for: ", play)
    next
  }
  
  # Create nodes
  words <- unique(c(word_cors$item1, word_cors$item2))
  
  nodes <- data.frame(
    id = words,
    label = words,
    color = "lightblue",
    title = paste("Word:", words),
    stringsAsFactors = FALSE
  )
  
  # Create edges
  edges <- data.frame(
    from = word_cors$item1,
    to = word_cors$item2,
    value = word_cors$correlation * 10,
    title = paste("Correlation:", round(word_cors$correlation, 3)),
    stringsAsFactors = FALSE
  )
  
  # Create interactive network
  network <- visNetwork(nodes, edges, main = paste("Word Correlations:", play)) %>%
    visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
    visPhysics(stabilization = TRUE) %>%
    visInteraction(navigationButtons = TRUE) %>%
    visLayout(randomSeed = 2016)
  
  # Save HTML file
  filename <- play %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_remove("^_|_$") %>%
    paste0(".html")
  
  visSave(network, file = file.path(OUTPUT_DIR_NETWORKS, filename))
  
  network_count <- network_count + 1
  message("  ✓ Saved: ", filename)
}

message("\n✓ Created ", network_count, " interactive network visualizations\n")

# Create index HTML file
message("Creating index page...")

index_html <- paste0(
  "<!DOCTYPE html>\n",
  "<html>\n",
  "<head>\n",
  "  <title>Shakespeare Interactive Networks</title>\n",
  "  <style>\n",
  "    body { font-family: Arial, sans-serif; margin: 40px; background-color: #f5f5f5; }\n",
  "    h1 { color: #333; }\n",
  "    ul { list-style-type: none; padding: 0; }\n",
  "    li { margin: 10px 0; }\n",
  "    a { color: #1a73e8; text-decoration: none; font-size: 18px; }\n",
  "    a:hover { text-decoration: underline; }\n",
  "    .container { background-color: white; padding: 30px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }\n",
  "  </style>\n",
  "</head>\n",
  "<body>\n",
  "  <div class='container'>\n",
  "    <h1>Shakespeare Interactive Word Networks</h1>\n",
  "    <p>Word correlation networks for Shakespeare's plays. Click a play to explore:</p>\n",
  "    <ul>\n"
)

# Add links to each play
for (play in sort(titles)) {
  filename <- play %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_remove("^_|_$") %>%
    paste0(".html")
  
  if (file.exists(file.path(OUTPUT_DIR_NETWORKS, filename))) {
    index_html <- paste0(index_html, "      <li><a href='", filename, "'>", play, "</a></li>\n")
  }
}

index_html <- paste0(
  index_html,
  "    </ul>\n",
  "    <hr>\n",
  "    <p><small>Generated: ", Sys.time(), "</small></p>\n",
  "  </div>\n",
  "</body>\n",
  "</html>"
)

writeLines(index_html, file.path(OUTPUT_DIR_NETWORKS, "index.html"))

message("✓ Saved index page: index.html\n")

# Summary
message(strrep("=", 70))
message("INTERACTIVE NETWORKS SUMMARY")
message(strrep("=", 70))
message("Networks created:     ", network_count)
message("Output directory:     ", OUTPUT_DIR_NETWORKS)
message("\nTo view:")
message("  Open: ", file.path(OUTPUT_DIR_NETWORKS, "index.html"))
message("  Or open individual HTML files directly")
message(strrep("=", 70), "\n")

message("✓ Interactive network generation complete!\n")
message("Open index.html in your browser to explore the networks.\n")