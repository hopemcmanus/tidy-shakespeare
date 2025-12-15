library(tidyverse)
library(tidytext)
library(here)

# Configuration
INPUT_DIR_CLEANED <- here("data", "cleaned")
INPUT_DIR_METADATA <- here("data", "metadata")
OUTPUT_DIR_PLOTS <- here("plots", "sentiment")

# Analysis settings
SENTIMENT_LEXICONS <- c("bing", "afinn", "nrc")  # All three lexicons for individual plays
GENRE_COMPARISON_LEXICON <- "bing"  # Use bing for genre comparison

# Example plays for genre comparison (one per genre)
EXAMPLE_PLAYS <- list(
  Comedy = "Twelfth Night",      # gutenberg_id 1526
  History = "Henry IV, Part 1",  # gutenberg_id 1516
  Tragedy = "Hamlet"             # gutenberg_id 1524
)

# Create output directories
dir.create(OUTPUT_DIR_PLOTS, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(OUTPUT_DIR_PLOTS, "individual_plays"), recursive = TRUE, showWarnings = FALSE)

message("\n", strrep("=", 70))
message("SHAKESPEARE SENTIMENT ANALYSIS")
message(strrep("=", 70))
message("Input: ", INPUT_DIR_CLEANED)
message("Output: ", OUTPUT_DIR_PLOTS)
message("Lexicons: ", paste(SENTIMENT_LEXICONS, collapse = ", "))
message(strrep("=", 70), "\n")

# Load data
message("Loading plays data...")
all_plays <- read_csv(
  file.path(INPUT_DIR_CLEANED, "all_shakespeare.csv"),
  show_col_types = FALSE
) %>%
  filter(class == "dialogue")

message("✓ Loaded ", format(nrow(all_plays), big.mark = ","), " dialogue lines\n")

# Part 1: Individual Play Sentiment Analysis (All Plays, All Lexicons)
message("PART 1: Individual Play Sentiment Analysis (All Plays)")
message(strrep("-", 70))

# Get list of all plays
all_play_titles <- all_plays %>%
  distinct(short_title) %>%
  pull(short_title) %>%
  sort()

message("Found ", length(all_play_titles), " plays to analyze\n")

play_sentiment_count <- 0

for (play_title in all_play_titles) {
  
  # Filter for single play
  single_play_data <- all_plays %>%
    filter(short_title == play_title) %>%
    mutate(
      # Clean act and scene
      act_clean = str_trim(str_remove(act, "\\.$")),
      scene_clean = str_trim(str_remove(scene, "\\.$")),
      # Extract Roman numerals
      act_num = str_extract(act_clean, "[IVXLCM]+$"),
      scene_num = str_extract(scene_clean, "[IVXLCM]+$"),
      # Combine for act-scene label
      act_scene = paste(act_num, scene_num, sep = ".")
    ) %>%
    # Tokenize
    unnest_tokens(word, text)
  
  # Calculate sentiment for all three lexicons
  sentiment_results <- list()
  
  for (lexicon in SENTIMENT_LEXICONS) {
    # Join sentiment
    play_with_sentiment <- single_play_data %>%
      inner_join(get_sentiments(lexicon), by = "word", relationship = "many-to-many")
    
    # Calculate sentiment by act-scene
    if (lexicon == "bing") {
      play_sentiment <- play_with_sentiment %>%
        count(act_scene, sentiment) %>%
        pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
        mutate(
          positive = if("positive" %in% names(.)) positive else 0,
          negative = if("negative" %in% names(.)) negative else 0,
          sentiment_score = positive - negative,
          lexicon = "Bing"
        ) %>%
        select(act_scene, sentiment_score, lexicon)
    } else if (lexicon == "afinn") {
      play_sentiment <- play_with_sentiment %>%
        group_by(act_scene) %>%
        summarise(sentiment_score = sum(value), .groups = "drop") %>%
        mutate(lexicon = "AFINN")
    } else if (lexicon == "nrc") {
      # NRC has multiple emotions; use positive/negative only
      play_sentiment <- play_with_sentiment %>%
        filter(sentiment %in% c("positive", "negative")) %>%
        count(act_scene, sentiment) %>%
        pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
        mutate(
          positive = if("positive" %in% names(.)) positive else 0,
          negative = if("negative" %in% names(.)) negative else 0,
          sentiment_score = positive - negative,
          lexicon = "NRC"
        ) %>%
        select(act_scene, sentiment_score, lexicon)
    }
    
    sentiment_results[[lexicon]] <- play_sentiment
  }
  
  # Combine all lexicons
  combined_sentiment <- bind_rows(sentiment_results) %>%
    mutate(lexicon = factor(lexicon, levels = c("Bing", "AFINN", "NRC")))
  
  # Plot: Faceted by lexicon
  p_play <- ggplot(combined_sentiment, aes(x = act_scene, y = sentiment_score, fill = sentiment_score)) +
    geom_col(width = 0.7, alpha = 0.9) +
    geom_hline(yintercept = 0, linewidth = 0.4, color = "gray30") +
    facet_wrap(~lexicon, ncol = 1, scales = "free_y") +
    scale_fill_gradient2(
      low = "#d73027",
      mid = "#ffffbf",
      high = "#4575b4",
      midpoint = 0,
      name = "Sentiment"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
      strip.text = element_text(face = "bold", size = 11),
      strip.background = element_rect(fill = "gray95", color = NA),
      plot.title = element_text(size = 14, face = "bold", hjust = 0),
      plot.subtitle = element_text(size = 10, color = "gray30", hjust = 0),
      plot.background = element_rect(fill = "white", color = NA),
      panel.spacing = unit(1, "lines")
    ) +
    labs(
      x = "Act - Scene",
      y = "Sentiment Score",
      title = paste("Sentiment Analysis:", play_title),
      subtitle = "Comparing Bing, AFINN, and NRC sentiment lexicons | Positive (blue) vs Negative (red)"
    )
  
  # Generate filename
  filename <- play_title %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_remove("^_|_$")
  
  ggsave(
    filename = paste0("sentiment_", filename, "_all_lexicons.png"),
    plot = p_play,
    path = file.path(OUTPUT_DIR_PLOTS, "individual_plays"),
    width = 12,
    height = 10,
    dpi = 600,
    bg = "white"
  )
  
  # Save CSV
  write_csv(
    combined_sentiment,
    file.path(OUTPUT_DIR_PLOTS, "individual_plays", paste0("sentiment_", filename, "_all_lexicons.csv"))
  )
  
  play_sentiment_count <- play_sentiment_count + 1
  
  if (play_sentiment_count %% 10 == 0) {
    message("  Analyzed ", play_sentiment_count, "/", length(all_play_titles), " plays...")
  }
}

message("✓ Created ", play_sentiment_count, " individual play sentiment analyses\n")

# Part 2: Genre Aggregate vs Example Plays
message("PART 2: Genre Aggregate Sentiment vs Example Plays")
message(strrep("-", 70))

# Tokenize all plays
message("Tokenizing all plays for aggregate analysis...")

all_plays_tokenized <- all_plays %>%
  mutate(
    act_num = str_extract(str_trim(str_remove(act, "\\.$")), "[IVXLCM]+$")
  ) %>%
  filter(!is.na(act_num)) %>%
  unnest_tokens(word, text)

message("✓ Tokenized ", format(nrow(all_plays_tokenized), big.mark = ","), " words")

# Calculate genre aggregate sentiment (normalized per 100 words)
message("Calculating genre aggregate sentiment...")

genre_sentiment <- all_plays_tokenized %>%
  group_by(gutenberg_id, genre, act_num) %>%
  mutate(total_words = n()) %>%
  ungroup() %>%
  inner_join(get_sentiments(GENRE_COMPARISON_LEXICON), by = "word", relationship = "many-to-many")

if (GENRE_COMPARISON_LEXICON == "bing") {
  genre_sentiment <- genre_sentiment %>%
    count(gutenberg_id, genre, act_num, sentiment, total_words) %>%
    pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
    mutate(
      positive = if("positive" %in% names(.)) positive else 0,
      negative = if("negative" %in% names(.)) negative else 0,
      sentiment_score = (positive - negative) / total_words * 100
    )
} else if (GENRE_COMPARISON_LEXICON == "afinn") {
  genre_sentiment <- genre_sentiment %>%
    group_by(gutenberg_id, genre, act_num, total_words) %>%
    summarise(sentiment_sum = sum(value), .groups = "drop") %>%
    mutate(sentiment_score = sentiment_sum / total_words * 100)
} else if (GENRE_COMPARISON_LEXICON == "nrc") {
  genre_sentiment <- genre_sentiment %>%
    filter(sentiment %in% c("positive", "negative")) %>%
    count(gutenberg_id, genre, act_num, sentiment, total_words) %>%
    pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
    mutate(
      positive = if("positive" %in% names(.)) positive else 0,
      negative = if("negative" %in% names(.)) negative else 0,
      sentiment_score = (positive - negative) / total_words * 100
    )
}

# Aggregate by genre
genre_aggregate <- genre_sentiment %>%
  group_by(genre, act_num) %>%
  summarise(sentiment_score = mean(sentiment_score), .groups = "drop") %>%
  mutate(
    type = "Aggregate",
    play = NA_character_
  )

message("✓ Calculated aggregate sentiment for ", n_distinct(genre_aggregate$genre), " genres")

# Calculate example play sentiment (normalized per 100 words)
message("Calculating example play sentiment...")

example_sentiment <- all_plays_tokenized %>%
  filter(short_title %in% unlist(EXAMPLE_PLAYS)) %>%
  mutate(
    genre = case_when(
      short_title == EXAMPLE_PLAYS$Comedy ~ "Comedy",
      short_title == EXAMPLE_PLAYS$History ~ "History",
      short_title == EXAMPLE_PLAYS$Tragedy ~ "Tragedy",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(gutenberg_id, short_title, genre, act_num) %>%
  mutate(total_words = n()) %>%
  ungroup() %>%
  inner_join(get_sentiments(GENRE_COMPARISON_LEXICON), by = "word", relationship = "many-to-many")

if (GENRE_COMPARISON_LEXICON == "bing") {
  example_sentiment <- example_sentiment %>%
    count(genre, short_title, act_num, sentiment, total_words) %>%
    pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
    mutate(
      positive = if("positive" %in% names(.)) positive else 0,
      negative = if("negative" %in% names(.)) negative else 0,
      sentiment_score = (positive - negative) / total_words * 100
    )
} else if (GENRE_COMPARISON_LEXICON == "afinn") {
  example_sentiment <- example_sentiment %>%
    group_by(genre, short_title, act_num, total_words) %>%
    summarise(sentiment_sum = sum(value), .groups = "drop") %>%
    mutate(sentiment_score = sentiment_sum / total_words * 100)
} else if (GENRE_COMPARISON_LEXICON == "nrc") {
  example_sentiment <- example_sentiment %>%
    filter(sentiment %in% c("positive", "negative")) %>%
    count(genre, short_title, act_num, sentiment, total_words) %>%
    pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
    mutate(
      positive = if("positive" %in% names(.)) positive else 0,
      negative = if("negative" %in% names(.)) negative else 0,
      sentiment_score = (positive - negative) / total_words * 100
    )
}

example_plays_data <- example_sentiment %>%
  mutate(
    type = "Example",
    play = short_title
  ) %>%
  select(genre, act_num, sentiment_score, type, play)

message("✓ Calculated sentiment for ", n_distinct(example_plays_data$play), " example plays\n")

# Combine aggregate and examples
combined_sentiment <- bind_rows(
  genre_aggregate,
  example_plays_data
) %>%
  mutate(
    facet_label = if_else(
      type == "Example",
      paste0(genre, " - ", play),
      paste0(genre, " - Aggregate")
    ),
    act_num = factor(act_num, levels = c("I", "II", "III", "IV", "V"))
  )

# Plot: Genre aggregate vs example plays
message("Creating genre comparison plot...")

p_combined <- ggplot(combined_sentiment, aes(x = act_num, y = sentiment_score, fill = sentiment_score)) +
  geom_col(width = 0.7, alpha = 0.9) +
  geom_hline(yintercept = 0, linewidth = 0.4, color = "gray30") +
  facet_wrap(~facet_label, ncol = 2, scales = "free_x") +
  scale_fill_gradient2(
    low = "#d73027",
    mid = "#ffffbf",
    high = "#4575b4",
    midpoint = 0,
    name = "Sentiment"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    strip.text = element_text(
      face = "bold",
      size = 12,
      margin = margin(b = 10)
    ),
    strip.background = element_rect(
      fill = "gray95",
      color = NA
    ),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
    panel.spacing = unit(1.5, "lines"),
    axis.text.x = element_text(size = 11, color = "gray20"),
    axis.text.y = element_text(size = 10, color = "gray20"),
    axis.title = element_text(size = 12, face = "bold", color = "gray10"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    plot.title = element_text(
      size = 16,
      face = "bold",
      hjust = 0,
      margin = margin(b = 5)
    ),
    plot.subtitle = element_text(
      size = 11,
      color = "gray30",
      hjust = 0,
      margin = margin(b = 15)
    ),
    plot.caption = element_text(
      size = 9,
      color = "gray50",
      hjust = 0,
      margin = margin(t = 15)
    ),
    plot.margin = margin(20, 20, 20, 20),
    legend.position = "right",
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  labs(
    x = "Act",
    y = "Sentiment Density (per 100 words)",
    title = "Sentiment Patterns Across Shakespeare's Dramatic Structure",
    subtitle = paste("Blue indicates positive sentiment, red indicates negative sentiment | Using", GENRE_COMPARISON_LEXICON, "lexicon"),
    caption = "Data: Project Gutenberg | Net sentiment words per 100 total words"
  )

ggsave(
  filename = "genre_aggregate_vs_examples.png",
  plot = p_combined,
  path = OUTPUT_DIR_PLOTS,
  width = 13,
  height = 10,
  dpi = 600,
  bg = "white"
)

message("✓ Saved genre comparison plot\n")

# Save analysis results
message("Saving analysis results...")

write_csv(
  combined_sentiment,
  file.path(OUTPUT_DIR_PLOTS, "genre_aggregate_sentiment.csv")
)

message("✓ Saved CSV files\n")

# Summary
message(strrep("=", 70))
message("SENTIMENT ANALYSIS SUMMARY")
message(strrep("=", 70))
message("\nAnalyses completed:")
message("  1. Individual plays: ", play_sentiment_count, " plays × 3 lexicons")
message("  2. Genre aggregates with examples")
message("\nPlots created:")
message("  - ", play_sentiment_count, " individual play plots (all lexicons)")
message("  - 1 genre comparison plot")
message("\nSentiment lexicons used:")
message("  Individual plays: ", paste(SENTIMENT_LEXICONS, collapse = ", "))
message("  Genre comparison: ", GENRE_COMPARISON_LEXICON)
message("\nExample plays:")
message("  Comedy:  ", EXAMPLE_PLAYS$Comedy)
message("  History: ", EXAMPLE_PLAYS$History)
message("  Tragedy: ", EXAMPLE_PLAYS$Tragedy)
message(strrep("=", 70))
message("Files saved to: ", OUTPUT_DIR_PLOTS)
message(strrep("=", 70), "\n")

message("✓ Sentiment analysis complete!\n")