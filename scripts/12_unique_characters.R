library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(text2map)

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

cat("Total rows loaded:", nrow(tokens), "\n")

# Extract and process unique characters WITHOUT pre-filtering
split_characters <- tokens %>%
  filter(!is.na(character), character != "") %>%
  # Remove collective entries starting with "all" or "both"
  filter(!str_detect(tolower(character), "^(all|both)")) %>%
  # Split on commas
  mutate(character = str_split(character, ",")) %>%
  unnest(character) %>%
  # Split on " and " (case-insensitive)
  mutate(character = str_split(character, regex("\\s+and\\s+", ignore_case = TRUE))) %>%
  unnest(character) %>%
  # Clean up whitespace
  mutate(character = str_trim(character)) %>%
  # Remove empty strings
  filter(character != "", !is.na(character))

cat("Characters after splitting:", n_distinct(split_characters$character), "\n")

# Create unique character list with full metadata
unique_characters <- split_characters %>%
  distinct(gutenberg_id, short_title, character) %>%
  arrange(gutenberg_id, character) %>%
  left_join(
    meta_shakespeare %>% select(gutenberg_id, short_title, genre, year),
    by = c("gutenberg_id", "short_title")
  )

# Count appearances per character per play
character_stats <- split_characters %>%
  group_by(gutenberg_id, short_title, character) %>%
  summarise(
    n_tokens = n(),
    n_scenes = n_distinct(scene),
    n_acts = n_distinct(act),
    .groups = "drop"
  ) %>%
  left_join(
    meta_shakespeare %>% select(gutenberg_id, short_title, genre, year),
    by = c("gutenberg_id", "short_title")
  )

# Characters per play
chars_per_play <- unique_characters %>%
  group_by(gutenberg_id, short_title, genre) %>%
  summarise(n_characters = n(), .groups = "drop") %>%
  arrange(desc(n_characters))

# Most common character names across all plays
common_names <- unique_characters %>%
  group_by(character) %>%
  summarise(
    n_plays = n(),
    plays = paste(short_title, collapse = "; "),
    .groups = "drop"
  ) %>%
  arrange(desc(n_plays)) %>%
  filter(n_plays > 1)

# Create metadata directory if it doesn't exist
meta_dir <- "data/metadata"
if(!dir.exists(meta_dir)) dir.create(meta_dir, recursive = TRUE)

# Save outputs
write_csv(unique_characters, file.path(meta_dir, "unique_characters.csv"))
cat("✓ Saved unique_characters.csv\n")
cat("  Total unique character-play combinations:", nrow(unique_characters), "\n")

write_csv(character_stats, file.path(meta_dir, "character_statistics.csv"))
cat("✓ Saved character_statistics.csv\n")

write_csv(chars_per_play, file.path(meta_dir, "characters_per_play.csv"))
cat("✓ Saved characters_per_play.csv\n")

write_csv(common_names, file.path(meta_dir, "common_character_names.csv"))
cat("✓ Saved common_character_names.csv\n")

cat("\n", strrep("=", 60), "\n")
cat("CHARACTER EXTRACTION SUMMARY\n")
cat(strrep("=", 60), "\n")

cat("\nTotal plays processed:", n_distinct(unique_characters$gutenberg_id), "\n")
cat("Total unique characters:", nrow(unique_characters), "\n")
cat("Average characters per play:", round(mean(chars_per_play$n_characters), 1), "\n")

cat("\nTop 10 plays by character count:\n")
print(chars_per_play %>% head(10), n = 10)

cat("\nMost common character names (appearing in multiple plays):\n")
print(common_names %>% head(10), n = 10)

cat("\n✓ Complete!\n")