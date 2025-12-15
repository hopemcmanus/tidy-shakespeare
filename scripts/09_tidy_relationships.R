library(tidyverse)
library(tidytext)
library(igraph)
library(ggraph)
library(widyr)
library(here)

# Configuration
INPUT_DIR_CLEANED <- here("data", "cleaned")
INPUT_DIR_METADATA <- here("data", "metadata")
OUTPUT_DIR_PLOTS <- here("plots", "relationships")

# Filter settings - Customize these to analyze different subsets
# FILTER_BY <- "gutenberg_id"  # Options: "gutenberg_id", "genre", "short_title", "period", "romance", "problem_play", "roman", "all"
# FILTER_VALUES <- c("1516", "1519", "1522", "1524","1523", "1503", "1540", "1531", "1515")  # HSC plays
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

# Create output directories
dir.create(OUTPUT_DIR_PLOTS, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(OUTPUT_DIR_PLOTS, "bigrams"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(OUTPUT_DIR_PLOTS, "networks"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(OUTPUT_DIR_PLOTS, "correlations"), recursive = TRUE, showWarnings = FALSE)

message("\n", strrep("=", 70))
message("SHAKESPEARE RELATIONSHIP ANALYSIS")
message(strrep("=", 70))
message("Input: ", INPUT_DIR_CLEANED)
message("Output: ", OUTPUT_DIR_PLOTS)
message("Filter: ", FILTER_BY, " = ", paste(FILTER_VALUES, collapse = ", "))
message(strrep("=", 70), "\n")

# Part 1: Bigram Analysis
message("PART 1: Bigram Analysis (Word Pairs)")
message(strrep("-", 70))

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

# Create bigrams
message("Creating bigrams...")

shakespeare_bigrams <- selected_plays %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram))

message("✓ Created ", format(nrow(shakespeare_bigrams), big.mark = ","), " bigrams")

# Separate and filter bigrams
message("Filtering bigrams (removing stop words)...")

bigrams_separated <- shakespeare_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words_custom$word,
         !word2 %in% stop_words_custom$word)

message("✓ Filtered to ", format(nrow(bigrams_filtered), big.mark = ","), " bigrams\n")

# Count bigrams
bigram_counts <- bigrams_filtered %>%
  count(short_title, word1, word2, sort = TRUE)

# Unite bigrams for TF-IDF
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

# Calculate bigram TF-IDF
message("Calculating bigram TF-IDF scores...")

bigram_tf_idf <- bigrams_united %>%
  count(short_title, bigram) %>%
  bind_tf_idf(bigram, short_title, n) %>%
  arrange(desc(tf_idf))

message("✓ Calculated TF-IDF for ", format(nrow(bigram_tf_idf), big.mark = ","), " unique bigrams\n")

# Display top bigrams
message("Top 20 bigrams by TF-IDF:")
print(bigram_tf_idf %>% head(20))
message("")

# Plot: Top TF-IDF bigrams by play
message("Creating bigram TF-IDF plot...")

p_bigram_tfidf <- bigram_tf_idf %>%
  group_by(short_title) %>%
  slice_max(tf_idf, n = 10) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(bigram, tf_idf), fill = short_title)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~short_title, ncol = 2, scales = "free") +
  labs(
    x = "TF-IDF",
    y = NULL,
    title = "Highest TF-IDF Bigrams by Play",
    subtitle = "Distinctive two-word phrases in each play"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold", size = 10))

ggsave(
  filename = "highest_tf_idf_bigrams.png",
  plot = p_bigram_tfidf,
  path = file.path(OUTPUT_DIR_PLOTS, "bigrams"),
  width = 10,
  height = 12,
  dpi = 600
)

message("✓ Saved: bigrams/highest_tf_idf_bigrams.png\n")

# Part 2: Bigram Network Graphs
message("PART 2: Bigram Network Graphs")
message(strrep("-", 70))

titles <- unique(bigram_counts$short_title)
network_count <- 0

for (play in titles) {
  bigram_graph <- bigram_counts %>%
    filter(short_title == play, n > 2) %>%
    select(word1, word2, n) %>%
    graph_from_data_frame()
  
  # Plot the graph
  set.seed(2020)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  p <- ggraph(bigram_graph, layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                   arrow = a, end_cap = circle(.07, 'inches')) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    labs(title = paste("Bigram Network:", play)) +
    theme_void()
  
  # Save plot
  filename <- play %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_remove("^_|_$")
  
  ggsave(
    filename = paste0("bigram_network_", filename, ".png"),
    plot = p,
    path = file.path(OUTPUT_DIR_PLOTS, "networks"),
    width = 10,
    height = 8,
    dpi = 300
  )
  
  network_count <- network_count + 1
  if (network_count %% 5 == 0) {
    message("  Created ", network_count, "/", length(titles), " network graphs...")
  }
}

message("✓ Saved ", network_count, " bigram network graphs\n")

# Part 3: Word Correlations by Section
message("PART 3: Word Correlations by Section (10-line chunks)")
message(strrep("-", 70))

# Prepare section-based data
message("Creating section-based word data...")

section_words <- selected_plays %>%
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
  filter(!word %in% stop_words_custom$word)

message("✓ Created section-based word data")
message("  Total words: ", format(nrow(hsc_section_words), big.mark = ","))

# Count words by section
section_word_counts <- section_words %>%
  count(short_title, section, word, sort = TRUE)

message("✓ Counted words by section\n")

# Create correlation networks for each play
message("Creating word correlation networks by section...")

correlation_count <- 0

for (play in titles) {
  play_data <- section_word_counts %>%
    filter(short_title == play)
  
  # Compute pairwise correlations
  word_cors <- play_data %>%
    add_count(word, name = "word_total") %>%
    filter(word_total >= 10) %>%
    pairwise_cor(word, section, value = n, sort = TRUE) %>%
    filter(correlation > 0.15)
  
  if (nrow(word_cors) == 0) {
    message("  No correlations above threshold for: ", play)
    next
  }
  
  # Build graph
  set.seed(2016)
  
  g <- word_cors %>%
    graph_from_data_frame(directed = FALSE)
  
  p <- ggraph(g, layout = "fr") +
    geom_edge_link(aes(edge_alpha = correlation),
                   edge_width = 0.3,
                   show.legend = FALSE) +
    geom_node_point(color = "lightblue", size = 4) +
    geom_node_text(aes(label = name), repel = TRUE) +
    labs(title = paste("Word Correlations by Section:", play),
         subtitle = "Words that appear together in 10-line sections") +
    theme_void()
  
  # Save plot
  filename <- play %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_remove("^_|_$")
  
  ggsave(
    filename = paste0("section_correlations_", filename, ".png"),
    plot = p,
    path = file.path(OUTPUT_DIR_PLOTS, "correlations"),
    width = 12,
    height = 10,
    dpi = 300
  )
  
  correlation_count <- correlation_count + 1
  if (correlation_count %% 5 == 0) {
    message("  Created ", correlation_count, "/", length(titles), " correlation graphs...")
  }
}

message("✓ Saved ", correlation_count, " section correlation graphs\n")

# Save analysis results
message("Saving analysis results...")

write_csv(
  bigram_counts,
  file.path(OUTPUT_DIR_PLOTS, "bigram_counts.csv")
)

write_csv(
  bigram_tf_idf,
  file.path(OUTPUT_DIR_PLOTS, "bigram_tf_idf.csv")
)

write_csv(
  section_word_counts,
  file.path(OUTPUT_DIR_PLOTS, "section_word_counts.csv")
)

message("✓ Saved CSV files\n")

# Summary
message(strrep("=", 70))
message("RELATIONSHIP ANALYSIS SUMMARY")
message(strrep("=", 70))
message("\nAnalyses completed:")
message("  1. Bigram analysis and TF-IDF")
message("  2. Bigram network graphs (", network_count, " plays)")
message("  3. Word correlation networks (", correlation_count, " plays)")
message("\nPlots created:")
message("  Bigrams:      1 TF-IDF plot")
message("  Networks:     ", network_count, " bigram network graphs")
message("  Correlations: ", correlation_count, " section correlation graphs")
message("\nTotal plots: ", 1 + network_count + correlation_count)
message(strrep("=", 70))
message("Files saved to: ", OUTPUT_DIR_PLOTS)
message(strrep("=", 70), "\n")

message("✓ Relationship analysis complete!\n")