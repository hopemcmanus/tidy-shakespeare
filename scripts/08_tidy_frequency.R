library(tidyverse)
library(tidytext)
library(wordcloud)
library(RColorBrewer)
library(here)

# Configuration
INPUT_DIR_FILTERED <- here("data", "processed", "filtered")
INPUT_DIR_CLEANED <- here("data", "cleaned")
OUTPUT_DIR_PLOTS <- here("plots", "frequency")
CHAR_NAMES_FILE <- here("data", "metadata", "all_characters.csv")  # Your character names file

# Create output directories
dir.create(OUTPUT_DIR_PLOTS, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(OUTPUT_DIR_PLOTS, "individual_plays"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(OUTPUT_DIR_PLOTS, "wordclouds"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(OUTPUT_DIR_PLOTS, "by_genre"), recursive = TRUE, showWarnings = FALSE)

message("\n", strrep("=", 70))
message("SHAKESPEARE FREQUENCY ANALYSIS")
message(strrep("=", 70))
message("Input: ", INPUT_DIR_FILTERED)
message("Output: ", OUTPUT_DIR_PLOTS)
message(strrep("=", 70), "\n")

# Load character names to exclude (optional)
message("Loading character names list...")
if (file.exists(CHAR_NAMES_FILE)) {
  all_chars_df <- read_csv(CHAR_NAMES_FILE, show_col_types = FALSE)
  
  # Create character names lookup (lowercase for matching)
  char_names <- all_chars_df %>%
    mutate(word = str_to_lower(character)) %>%
    distinct(word) %>%
    pull(word)
  
  message("✓ Loaded ", length(char_names), " character names to exclude")
  exclude_chars <- TRUE
} else {
  message("! Character names file not found: ", CHAR_NAMES_FILE)
  message("  Proceeding without character name exclusion")
  exclude_chars <- FALSE
  char_names <- c()
}
message("")

# Optional: Load character exclusion list
# Uncomment and specify path to use character filtering
# all_chars_df <- read_csv("path/to/all_chars_df.csv", show_col_types = FALSE)
# characters_to_exclude <- c("HAMLET", "OPHELIA")  # Specify which characters to exclude

# Helper function to filter out excluded characters
apply_character_filter <- function(data, exclude_chars = NULL) {
  if (!is.null(exclude_chars) && length(exclude_chars) > 0) {
    message("Excluding characters: ", paste(exclude_chars, collapse = ", "))
    data <- data %>%
      filter(!character %in% exclude_chars)
  }
  return(data)
}

# Step 1: Individual play TF-IDF analysis (Words and Bigrams)
message("STEP 1: Individual Play TF-IDF Analysis (All Plays)")
message(strrep("-", 70))

# Load all filtered play tokens for word TF-IDF
message("Loading all filtered play tokens...")

all_filtered_tokens <- tibble()
play_files <- list.files(file.path(INPUT_DIR_FILTERED, "by_play"), 
                         pattern = "_filtered\\.csv$", full.names = TRUE)

for (play_file in play_files) {
  play_data <- read_csv(play_file, show_col_types = FALSE)
  all_filtered_tokens <- bind_rows(all_filtered_tokens, play_data)
}

message("✓ Loaded ", format(nrow(all_filtered_tokens), big.mark = ","), " tokens from ", 
        n_distinct(all_filtered_tokens$short_title), " plays\n")

# Calculate word TF-IDF
message("Calculating word TF-IDF scores...")

word_counts <- all_filtered_tokens %>%
  count(short_title, word, sort = TRUE)

word_tf_idf <- word_counts %>%
  bind_tf_idf(word, short_title, n) %>%
  arrange(short_title, desc(tf_idf))

message("✓ Calculated word TF-IDF\n")

# Load cleaned data for bigram TF-IDF
message("Loading cleaned data for bigram analysis...")

all_plays_cleaned <- read_csv(
  file.path(INPUT_DIR_CLEANED, "all_shakespeare.csv"),
  show_col_types = FALSE
) %>%
  filter(class == "dialogue")

message("✓ Loaded ", format(nrow(all_plays_cleaned), big.mark = ","), " dialogue lines")

# Create bigrams
message("Creating and filtering bigrams...")

shakespeare_bigrams <- all_plays_cleaned %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram))

# Filter stop words from bigrams
bigrams_separated <- shakespeare_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words_custom$word,
         !word2 %in% stop_words_custom$word)

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

# Calculate bigram TF-IDF
bigram_counts <- bigrams_united %>%
  count(short_title, bigram, sort = TRUE)

bigram_tf_idf <- bigram_counts %>%
  bind_tf_idf(bigram, short_title, n) %>%
  arrange(short_title, desc(tf_idf))

message("✓ Calculated bigram TF-IDF\n")

# Create plots for each play
message("Creating TF-IDF plots for each play...")

play_titles <- sort(unique(word_tf_idf$short_title))
plot_count <- 0

for (play_title in play_titles) {
  
  # Generate filename
  filename <- play_title %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_remove("^_|_$")
  
  # Word TF-IDF plot
  top_words <- word_tf_idf %>%
    filter(short_title == play_title) %>%
    slice_max(tf_idf, n = 20) %>%
    mutate(
      # Capitalize if character name
      word_display = if_else(word %in% char_names, str_to_title(word), word)
    )
  
  p_words <- top_words %>%
    mutate(word_display = reorder(word_display, tf_idf)) %>%
    ggplot(aes(tf_idf, word_display)) +
    geom_col(fill = "steelblue", alpha = 0.8) +
    labs(
      x = "TF-IDF",
      y = NULL,
      title = paste("Most Distinctive Words:", play_title),
      subtitle = "Top 20 words by TF-IDF (frequent here, rare elsewhere)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 10, color = "gray30")
    )
  
  ggsave(
    filename = paste0(filename, "_word_tfidf.png"),
    plot = p_words,
    path = file.path(OUTPUT_DIR_PLOTS, "individual_plays"),
    width = 8,
    height = 6,
    dpi = 600
  )
  
  # Bigram TF-IDF plot
  top_bigrams <- bigram_tf_idf %>%
    filter(short_title == play_title) %>%
    slice_max(tf_idf, n = 20)
  
  p_bigrams <- top_bigrams %>%
    mutate(bigram = reorder(bigram, tf_idf)) %>%
    ggplot(aes(tf_idf, bigram)) +
    geom_col(fill = "darkorange", alpha = 0.8) +
    labs(
      x = "TF-IDF",
      y = NULL,
      title = paste("Most Distinctive Bigrams:", play_title),
      subtitle = "Top 20 bigrams by TF-IDF (frequent here, rare elsewhere)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 10, color = "gray30")
    )
  
  ggsave(
    filename = paste0(filename, "_bigram_tfidf.png"),
    plot = p_bigrams,
    path = file.path(OUTPUT_DIR_PLOTS, "individual_plays"),
    width = 8,
    height = 6,
    dpi = 600
  )
  
  plot_count <- plot_count + 1
  
  if (plot_count %% 10 == 0) {
    message("  Created ", plot_count, "/", length(play_titles), " play analyses...")
  }
}

message("✓ Created ", plot_count, " play analyses (words + bigrams)\n")

# Save TF-IDF data
write_csv(word_tf_idf, file.path(OUTPUT_DIR_PLOTS, "all_plays_word_tfidf.csv"))
write_csv(bigram_tf_idf, file.path(OUTPUT_DIR_PLOTS, "all_plays_bigram_tfidf.csv"))

message("✓ Saved TF-IDF data files\n")


# Step 1b: Individual play TF-IDF analysis (Words and Bigrams)
message("STEP 1b: Individual Play TF-IDF Analysis (All Plays)")
message(strrep("-", 70))

# Load all filtered play tokens for word TF-IDF
message("Loading all filtered play tokens for TF-IDF...")

all_filtered_tokens <- tibble()
play_files <- list.files(file.path(INPUT_DIR_FILTERED, "by_play"), 
                         pattern = "_filtered\\.csv$", full.names = TRUE)

for (play_file in play_files) {
  play_data <- read_csv(play_file, show_col_types = FALSE)
  all_filtered_tokens <- bind_rows(all_filtered_tokens, play_data)
}

message("✓ Loaded ", format(nrow(all_filtered_tokens), big.mark = ","), " tokens from ", 
        n_distinct(all_filtered_tokens$short_title), " plays\n")

# Calculate word TF-IDF
message("Calculating word TF-IDF scores...")

word_counts <- all_filtered_tokens %>%
  count(short_title, word, sort = TRUE)

word_tf_idf <- word_counts %>%
  bind_tf_idf(word, short_title, n) %>%
  arrange(short_title, desc(tf_idf))

message("✓ Calculated word TF-IDF\n")

# Load cleaned data for bigram TF-IDF
message("Loading cleaned data for bigram analysis...")

all_plays_cleaned <- read_csv(
  file.path(INPUT_DIR_CLEANED, "all_shakespeare.csv"),
  show_col_types = FALSE
) %>%
  filter(class == "dialogue")

message("✓ Loaded ", format(nrow(all_plays_cleaned), big.mark = ","), " dialogue lines")

# Create bigrams
message("Creating and filtering bigrams...")

shakespeare_bigrams <- all_plays_cleaned %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram))

# Filter stop words from bigrams
bigrams_separated <- shakespeare_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words_custom$word,
         !word2 %in% stop_words_custom$word)

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

# Calculate bigram TF-IDF
bigram_counts <- bigrams_united %>%
  count(short_title, bigram, sort = TRUE)

bigram_tf_idf <- bigram_counts %>%
  bind_tf_idf(bigram, short_title, n) %>%
  arrange(short_title, desc(tf_idf))

message("✓ Calculated bigram TF-IDF\n")

# Create TF-IDF plots for each play
message("Creating TF-IDF plots for each play...")

play_titles_tfidf <- sort(unique(word_tf_idf$short_title))
tfidf_plot_count <- 0

for (play_title in play_titles_tfidf) {
  
  # Generate filename
  filename <- play_title %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_remove("^_|_$")
  
  # Word TF-IDF plot
  top_words <- word_tf_idf %>%
    filter(short_title == play_title) %>%
    slice_max(tf_idf, n = 20)
  
  p_words <- top_words %>%
    mutate(word = reorder(word, tf_idf)) %>%
    ggplot(aes(tf_idf, word)) +
    geom_col(fill = "steelblue", alpha = 0.8) +
    labs(
      x = "TF-IDF",
      y = NULL,
      title = paste("Most Distinctive Words:", play_title),
      subtitle = "Top 20 words by TF-IDF (frequent here, rare elsewhere)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 10, color = "gray30")
    )
  
  ggsave(
    filename = paste0(filename, "_word_tfidf.png"),
    plot = p_words,
    path = file.path(OUTPUT_DIR_PLOTS, "individual_plays"),
    width = 8,
    height = 6,
    dpi = 600
  )
  
  # Bigram TF-IDF plot
  top_bigrams <- bigram_tf_idf %>%
    filter(short_title == play_title) %>%
    slice_max(tf_idf, n = 20)
  
  if (nrow(top_bigrams) > 0) {
    p_bigrams <- top_bigrams %>%
      mutate(bigram = reorder(bigram, tf_idf)) %>%
      ggplot(aes(tf_idf, bigram)) +
      geom_col(fill = "darkorange", alpha = 0.8) +
      labs(
        x = "TF-IDF",
        y = NULL,
        title = paste("Most Distinctive Bigrams:", play_title),
        subtitle = "Top 20 bigrams by TF-IDF (frequent here, rare elsewhere)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 10, color = "gray30")
      )
    
    ggsave(
      filename = paste0(filename, "_bigram_tfidf.png"),
      plot = p_bigrams,
      path = file.path(OUTPUT_DIR_PLOTS, "individual_plays"),
      width = 8,
      height = 6,
      dpi = 600
    )
  }
  
  tfidf_plot_count <- tfidf_plot_count + 1
  
  if (tfidf_plot_count %% 10 == 0) {
    message("  Created ", tfidf_plot_count, "/", length(play_titles_tfidf), " TF-IDF analyses...")
  }
}

message("✓ Created ", tfidf_plot_count, " TF-IDF analyses (words + bigrams)\n")

# Save TF-IDF data
write_csv(word_tf_idf, file.path(OUTPUT_DIR_PLOTS, "individual_plays", "all_plays_word_tfidf.csv"))
write_csv(bigram_tf_idf, file.path(OUTPUT_DIR_PLOTS, "individual_plays", "all_plays_bigram_tfidf.csv"))

message("✓ Saved TF-IDF data files\n")

# Step 2: Word clouds
message("STEP 2: Word Cloud Analysis")
message(strrep("-", 70))

# Word cloud for Hamlet
message("Creating word cloud for Hamlet...")

png(
  filename = file.path(OUTPUT_DIR_PLOTS, "wordclouds", "hamlet_wordcloud.png"),
  width = 800,
  height = 800,
  res = 150
)

hamlet_counts %>%
  with(wordcloud(
    word, n,
    max.words = 100,
    random.order = FALSE,
    colors = brewer.pal(8, "Dark2")
  ))

dev.off()
message("✓ Saved: hamlet_wordcloud.png")

# Word cloud for all HSC plays combined
message("Creating word cloud for all HSC plays...")

hsc_filtered <- read_csv(
  file.path(INPUT_DIR_FILTERED, "by_subset", "hsc_dialogue_filtered.csv"),
  show_col_types = FALSE
)

hsc_counts <- hsc_filtered %>%
  count(word, sort = TRUE)

png(
  filename = file.path(OUTPUT_DIR_PLOTS, "wordclouds", "hsc_plays_wordcloud.png"),
  width = 800,
  height = 800,
  res = 150
)

hsc_counts %>%
  with(wordcloud(
    word, n,
    max.words = 100,
    random.order = FALSE,
    colors = brewer.pal(8, "Dark2")
  ))

dev.off()
message("✓ Saved: hsc_plays_wordcloud.png\n")

# Step 3: Analysis by genre
message("STEP 3: Analysis by Genre (Comedies, Histories, Tragedies)")
message(strrep("-", 70))

# Load genre-specific filtered data
message("Loading genre-filtered datasets...")

tragedies <- read_csv(
  file.path(INPUT_DIR_FILTERED, "by_stopwords", "tragedies_no_stopwords.csv"),
  show_col_types = FALSE
) %>%
  filter(class == "dialogue")

comedies <- read_csv(
  file.path(INPUT_DIR_FILTERED, "by_genre", "comedies.csv"),
  show_col_types = FALSE
) %>%
  filter(class == "dialogue") %>%
  anti_join(tidytext::stop_words, by = "word")

histories <- read_csv(
  file.path(INPUT_DIR_FILTERED, "by_genre", "histories.csv"),
  show_col_types = FALSE
) %>%
  filter(class == "dialogue") %>%
  anti_join(tidytext::stop_words, by = "word")

message("✓ Loaded tragedies: ", format(nrow(tragedies), big.mark = ","), " tokens")
message("✓ Loaded comedies:  ", format(nrow(comedies), big.mark = ","), " tokens")
message("✓ Loaded histories: ", format(nrow(histories), big.mark = ","), " tokens")

# Count words by genre
tragedy_counts <- tragedies %>% count(word, sort = TRUE)
comedy_counts <- comedies %>% count(word, sort = TRUE)
history_counts <- histories %>% count(word, sort = TRUE)

# Plot top words by genre
message("\nCreating genre comparison plots...")

# Tragedies
p_tragedy <- tragedy_counts %>%
  head(20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col(fill = "#8B0000") +
  labs(
    x = "Frequency",
    y = NULL,
    title = "Most Frequent Words in Tragedies",
    subtitle = "Top 20 words"
  ) +
  theme_minimal()

ggsave(
  filename = "tragedies_top_words.png",
  plot = p_tragedy,
  path = file.path(OUTPUT_DIR_PLOTS, "by_genre"),
  width = 8,
  height = 6,
  dpi = 600
)

# Comedies
p_comedy <- comedy_counts %>%
  head(20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col(fill = "#FFD700") +
  labs(
    x = "Frequency",
    y = NULL,
    title = "Most Frequent Words in Comedies",
    subtitle = "Top 20 words"
  ) +
  theme_minimal()

ggsave(
  filename = "comedies_top_words.png",
  plot = p_comedy,
  path = file.path(OUTPUT_DIR_PLOTS, "by_genre"),
  width = 8,
  height = 6,
  dpi = 600
)

# Histories
p_history <- history_counts %>%
  head(20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col(fill = "#4169E1") +
  labs(
    x = "Frequency",
    y = NULL,
    title = "Most Frequent Words in Histories",
    subtitle = "Top 20 words"
  ) +
  theme_minimal()

ggsave(
  filename = "histories_top_words.png",
  plot = p_history,
  path = file.path(OUTPUT_DIR_PLOTS, "by_genre"),
  width = 8,
  height = 6,
  dpi = 600
)

message("✓ Saved genre bar charts")

# Combined genre comparison
message("Creating combined genre comparison plot...")

genre_combined <- bind_rows(
  tragedy_counts %>% mutate(genre = "Tragedy"),
  comedy_counts %>% mutate(genre = "Comedy"),
  history_counts %>% mutate(genre = "History")
) %>%
  group_by(genre) %>%
  slice_max(n, n = 15) %>%
  ungroup()

p_genre_compare <- genre_combined %>%
  mutate(word = reorder_within(word, n, genre)) %>%
  ggplot(aes(n, word, fill = genre)) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  facet_wrap(~genre, ncol = 3, scales = "free_y") +
  scale_fill_manual(values = c("Tragedy" = "#8B0000", "Comedy" = "#FFD700", "History" = "#4169E1")) +
  labs(
    x = "Frequency",
    y = NULL,
    title = "Top 15 Words by Genre",
    subtitle = "Comparing vocabulary across Shakespeare's genres"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold", size = 12))

ggsave(
  filename = "genre_comparison.png",
  plot = p_genre_compare,
  path = file.path(OUTPUT_DIR_PLOTS, "by_genre"),
  width = 12,
  height = 6,
  dpi = 600
)

message("✓ Saved: genre_comparison.png")

# Word clouds by genre
message("\nCreating word clouds by genre...")

png(
  filename = file.path(OUTPUT_DIR_PLOTS, "wordclouds", "tragedies_wordcloud.png"),
  width = 800,
  height = 800,
  res = 150
)
tragedy_counts %>%
  with(wordcloud(word, n, max.words = 100, random.order = FALSE,
                 colors = brewer.pal(8, "Reds")))
dev.off()

png(
  filename = file.path(OUTPUT_DIR_PLOTS, "wordclouds", "comedies_wordcloud.png"),
  width = 800,
  height = 800,
  res = 150
)
comedy_counts %>%
  with(wordcloud(word, n, max.words = 100, random.order = FALSE,
                 colors = brewer.pal(8, "YlOrRd")))
dev.off()

png(
  filename = file.path(OUTPUT_DIR_PLOTS, "wordclouds", "histories_wordcloud.png"),
  width = 800,
  height = 800,
  res = 150
)
history_counts %>%
  with(wordcloud(word, n, max.words = 100, random.order = FALSE,
                 colors = brewer.pal(8, "Blues")))
dev.off()

message("✓ Saved genre word clouds\n")

# Step 4: Comparative frequency analysis
message("STEP 4: Comparative Frequency Analysis")
message(strrep("-", 70))

# Compare all HSC plays
hsc_play_words <- hsc_filtered %>%
  group_by(short_title, word) %>%
  summarise(n = n(), .groups = "drop")

# Calculate total words per play
hsc_total_words <- hsc_play_words %>%
  group_by(short_title) %>%
  summarise(total = sum(n), .groups = "drop")

# Join and calculate frequencies
hsc_play_words <- hsc_play_words %>%
  left_join(hsc_total_words, by = "short_title")

# Term frequency distribution
message("Creating term frequency distribution for all HSC plays...")

p_tf_dist <- hsc_play_words %>%
  ggplot(aes(n/total, fill = short_title)) +
  geom_histogram(show.legend = FALSE, bins = 30) +
  xlim(NA, 0.0009) +
  facet_wrap(~short_title, ncol = 2, scales = "free_y") +
  labs(
    x = "Term Frequency (n/total)",
    y = "Count",
    title = "Term Frequency Distribution by Play",
    subtitle = "HSC prescribed Shakespeare plays"
  ) +
  theme_minimal()

ggsave(
  filename = "term_frequency_distribution.png",
  plot = p_tf_dist,
  path = OUTPUT_DIR_PLOTS,
  width = 10,
  height = 12,
  dpi = 600
)

message("✓ Saved: term_frequency_distribution.png")

# Zipf's law analysis
message("Calculating Zipf's law rankings...")

freq_by_rank <- hsc_play_words %>%
  group_by(short_title) %>%
  mutate(
    rank = row_number(desc(n)),
    term_frequency = n / total
  ) %>%
  ungroup()

p_zipf <- freq_by_rank %>%
  ggplot(aes(rank, term_frequency, colour = short_title)) +
  geom_line(linewidth = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    x = "Rank (log scale)",
    y = "Term Frequency (log scale)",
    title = "Zipf's Law in Shakespeare",
    subtitle = "Word frequency vs rank follows power law distribution"
  ) +
  theme_minimal()

ggsave(
  filename = "zipfs_law.png",
  plot = p_zipf,
  path = OUTPUT_DIR_PLOTS,
  width = 10,
  height = 8,
  dpi = 600
)

message("✓ Saved: zipfs_law.png")

# Fit Zipf's law model
rank_subset <- freq_by_rank %>%
  filter(rank < 500, rank > 10)

zipf_model <- lm(log10(term_frequency) ~ log10(rank), data = rank_subset)
zipf_coef <- coef(zipf_model)

p_zipf_fitted <- freq_by_rank %>%
  ggplot(aes(rank, term_frequency, colour = short_title)) +
  geom_abline(
    intercept = zipf_coef[1],
    slope = zipf_coef[2],
    colour = "gray50",
    linetype = 2,
    linewidth = 1
  ) +
  geom_line(linewidth = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    x = "Rank (log scale)",
    y = "Term Frequency (log scale)",
    title = "Zipf's Law with Fitted Exponent",
    subtitle = sprintf("log(frequency) = %.2f + %.2f × log(rank)", zipf_coef[1], zipf_coef[2])
  ) +
  theme_minimal()

ggsave(
  filename = "zipfs_law_fitted.png",
  plot = p_zipf_fitted,
  path = OUTPUT_DIR_PLOTS,
  width = 10,
  height = 8,
  dpi = 600
)

message("✓ Saved: zipfs_law_fitted.png")

# TF-IDF analysis
message("\nCalculating TF-IDF scores...")

hsc_tf_idf <- hsc_play_words %>%
  bind_tf_idf(word, short_title, n) %>%
  arrange(desc(tf_idf))

p_tfidf <- hsc_tf_idf %>%
  group_by(short_title) %>%
  slice_max(tf_idf, n = 10) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = short_title)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~short_title, ncol = 2, scales = "free") +
  labs(
    x = "TF-IDF",
    y = NULL,
    title = "Highest TF-IDF Words by Play",
    subtitle = "Words that are distinctive to each play"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold", size = 10))

ggsave(
  filename = "highest_tf_idf_words.png",
  plot = p_tfidf,
  path = OUTPUT_DIR_PLOTS,
  width = 10,
  height = 12,
  dpi = 600
)

message("✓ Saved: highest_tf_idf_words.png\n")

# Save analysis results
message("Saving analysis results...")

write_csv(hamlet_counts, file.path(OUTPUT_DIR_PLOTS, "hamlet_word_frequencies.csv"))
write_csv(hsc_play_words, file.path(OUTPUT_DIR_PLOTS, "hsc_word_frequencies.csv"))
write_csv(hsc_tf_idf, file.path(OUTPUT_DIR_PLOTS, "hsc_tf_idf_scores.csv"))

write_csv(
  tibble(
    model = "Zipf's Law",
    intercept = zipf_coef[1],
    slope = zipf_coef[2],
    r_squared = summary(zipf_model)$r.squared
  ),
  file.path(OUTPUT_DIR_PLOTS, "zipf_model_summary.csv")
)

message("✓ Saved CSV files\n")

# Summary
message(strrep("=", 70))
message("FREQUENCY ANALYSIS SUMMARY")
message(strrep("=", 70))
message("\nAnalyses completed:")
message("  1. Individual play (Hamlet)")
message("  2. Word clouds (6 clouds)")
message("  3. Genre comparison (Tragedies, Comedies, Histories)")
message("  4. Comparative frequency (HSC plays)")
message("\nPlots created:")
message("  Individual plays:  1 bar chart")
message("  Word clouds:       6 word clouds")
message("  By genre:          4 bar charts")
message("  Comparative:       3 plots (TF dist, Zipf, TF-IDF)")
message("\nTotal plots: 14")
message(strrep("=", 70))
message("Files saved to: ", OUTPUT_DIR_PLOTS)
message(strrep("=", 70), "\n")

message("✓ Frequency analysis complete!\n")