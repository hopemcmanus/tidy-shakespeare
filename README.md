## Description

This repository contains data on 37 plays of William Shakespeare, prepared for tidy text analysis.

## Data

The texts were accessed via [Project Gutenberg](https://www.gutenberg.org/) and processed into CSV format. For each work obtained from Project Gutenberg, the Plain Text UTF-8 edition is identified as being in the public domain. The texts reproduced here are based on those editions and therefore retain the same public-domain status.

## Pipeline

The project uses a pipeline with clear separation of concerns:

### Setup, Cleaning, Tokenising, Unnesting and Filtering

**00_setup_metadata.R**\
**Description:** Shakespeare metadata with custom classifications\
**Outputs:** Enhanced `meta_shakespeare.csv` with period (Elizabethan/Jacobean), romance, problem_play, and roman classifications

**01_download_gutenberg.R**\
**Description:** Downloads plays from Project Gutenberg with validation, caching, and provenance tracking\
**Outputs:** Raw text files in `data/raw/`, download logs with checksums\
**Features:** Retry logic, file validation, comprehensive error handling

**02_clean_shakespeare_structure.R**\
**Description:** Parses raw text into structured format with dialogue, directions, and references\
**Outputs:** Cleaned CSV files in `data/cleaned/` with act/scene/character attribution\
**Features:** Character name detection, stage direction parsing, apostrophe normalisation

**03_validate_parsing.R**\
**Description:** Runs 15 comprehensive quality checks on parsed data\
**Outputs:** Validation reports with ERROR/WARNING/INFO classifications\
**Features:** Orphan dialogue detection, character attribution validation, structural integrity checks

**04_tokenise.R**\
**Description:** Converts cleaned text to word tokens with apostrophe normalisation\
**Outputs:** Token files in `data/processed/tokens/` (one word per row with metadata)\
**Features:** Preserves all metadata (play, genre, act, scene, character, line number)

**05_filter.R**\
**Description:** Creates multiple filtered datasets for different analytical approaches\
**Outputs:** 20+ filtered datasets organised by content type, genre, play, stopwords, character, and custom subsets\
**Categories:** - `by_content/` - dialogue_only, directions_only, references_only - `by_play/` - Individual play files (with and without stop words) - `by_genre/` - tragedies, comedies, histories, late_romances, problem_plays, roman_plays - `by_stopwords/` - Filtered versions excluding custom Shakespearean stop words - `by_character/` - Individual character speech (e.g., hamlet_character, lady_macbeth_character) - `by_subset/` - Custom subsets (e.g., HSC prescribed plays)

**06_export_json.R**\
**Description:** Exports data to JSON format for web applications and APIs\
**Outputs:** Individual play JSONs, combined JSON, metadata JSONs, index files

**07_export_sqlite.R**\
**Description:** Creates SQLite database with indexed tables and pre-built views\
**Outputs:** `shakespeare.sqlite` with main plays table, metadata, individual play tables, and 5 convenience views\
**Features:** Performance-optimised indexes, play statistics table, example queries

### Tidy Text Analysis

**08_frequency_analysis.R**\
**Description:** Comprehensive word frequency and TF-IDF analysis\
**Analyses:** - Individual play analysis (e.g., Hamlet top words) - Word clouds (6 variations by genre and subset) - Genre comparisons (Tragedies, Comedies, Histories) - Term frequency distributions - Zipf's law analysis with fitted models - TF-IDF distinctive terms by play\
**Outputs:** 14 plots + 4 CSV files in `plots/frequency/`

**09_relationship_analysis.R**\
**Description:** Bigram analysis and word correlation networks\
**Analyses:** - Bigram frequency and TF-IDF - Bigram network graphs (directed word-pair networks) - Section-based word correlations (10-line chunks)\
**Configuration:** Flexible filtering by gutenberg_id, genre, short_title, period, romance, problem_play, roman, or all\
**Outputs:** \~20 network plots + 3 CSV files in `plots/relationships/`

**10_interactive_networks.R**\
**Description:** Creates interactive HTML network visualisations using visNetwork\
**Features:** Click-and-drag nodes, zoom, highlight nearest connections, node selection, navigation buttons\
**Configuration:** Same flexible filtering as script 09\
**Outputs:** Individual HTML files per play + index.html in `interactive_networks/`\

**11_sentiment_analysis.R**\
**Description:** Sentiment analysis using tidytext sentiment lexicons\
**Analyses:** - Single play sentiment by act-scene - Genre aggregate sentiment vs. example plays (normalised per 100 words)\
**Configuration:** - Choice of sentiment lexicon: bing, afinn, or nrc - Customisable single play and example plays - Color-coded visualisations (red = negative, blue = positive)\
**Outputs:** 2 plots + 2 CSV files in `plots/sentiment/`\

**12_unique_characters.R**\
**Description:** Extracts all unique characters from tokenized Shakespeare texts with comprehensive metadata\
**Analyses:**\
- Processes character column to identify all character names\
- Splits compound character entries (e.g., "Character1 and Character2")\
- Filters out collective entries (e.g., "All", "Both")\
- Joins with `meta_shakespeare` for genre, year, and play information\
- Generates character statistics (tokens, scenes appeared)\
**Outputs:** 4 CSV files in `data/metadata/`\
- `unique_characters.csv` – Complete character list\
- `character_statistics.csv` – Appearance statistics\
- `characters_per_play.csv` – Characters per play summary\
- `common_character_names.csv` – Names appearing in multiple plays\

**13_character_networks.R**\
**Description:** Builds character co-occurrence networks and generates static visualizations\
**Analyses:**\
- Creates networks where edges represent scene co-occurrences\
- Calculates network metrics (density, diameter, transitivity, modularity)\
- Detects communities using the Louvain algorithm\
- Generates high-resolution PNG plots with size and colour encoding\
- Exports networks in GraphML format for external analysis\
**Configuration:**\
- Node size represents degree centrality\
- Node colour indicates community membership\
- Edge width represents co-occurrence frequency\
- Minimum co-occurrence threshold = 2 scenes\
**Outputs:**\
- Static network visualizations in `plots/character_networks/*.png`\
- Network files in `data/networks/*.graphml` (Gephi, Cytoscape compatible)\
- Network statistics in `data/networks/network_metrics.csv`\

**14_interactive_character_networks.R**\
**Description:** Creates interactive, web-based network visualizations\
**Analyses:**\
- Generates HTML files for each play using visNetwork\
- Enables interactive exploration (zoom, pan, select)\
- Displays character statistics on hover\
- Allows filtering by community\
- Creates an index page linking all networks\
**Configuration:**\
- Hover over nodes to see character details\
- Click to highlight connected characters\
- Search for specific characters\
- Filter by community\
- Navigation buttons for zooming/panning\
**Outputs:**\
- Individual play networks in `interactive_networks/characters/*.html`\
- Master index page at `interactive_networks/characters/index.html`\

## Interactive Networks: Characters

### Network Construction

-   **Nodes:** Individual characters
-   **Edges:** Scene co-occurrences (appearing together in the same scene)
-   **Edge weight:** Number of scenes shared
-   **Minimum threshold:** 2 co-occurrences (filters noise)

### Network Metrics

**Node-level metrics:** - **Degree:** Number of direct connections (characters interacted with) - **Betweenness:** How often a character lies on paths between others - **Closeness:** How quickly a character can reach all others - **Community:** Cluster of closely connected characters

**Network-level metrics:** - **Density:** Proportion of possible edges that exist - **Diameter:** Longest shortest path between any two characters - **Transitivity:** Clustering coefficient (friend-of-friend connections) - **Modularity:** Strength of community structure

## Legacy Analysis Scripts

The following scripts demonstrate text analysis approaches adapted from *Tidy Text Mining with R* by Silge & Robinson (2017):

**tidy_sentiment.R**\
**Description:** Performs sentiment analysis on tidy CSV data\
**Source:** Adapted from [Chapter 2 Sentiment Analysis with Tidy Data](https://www.tidytextmining.com/sentiment.html)

**tidy_frequency.R**\
**Description:** Performs tf-idf word/document frequency analysis\
**Source:** Adapted from [Chapter 3 Word and Document Frequency: tf-idf](https://www.tidytextmining.com/tfidf.html)

**tidy_relationships.R**\
**Description:** Extracts n-grams and calculates word correlations to explore frequently co-occurring words and relationships between terms\
**Source:** Adapted from [Chapter 4 Relationships Between Words: n-grams and Correlations](https://www.tidytextmining.com/ngrams.html)

**cleangutenbergshakespeare.R**\
**Description:** Original monolithic cleaning script (replaced by modular pipeline)

## Project Structure

```         
shakespeare_analysis/
├── scripts/
│   ├── 00_setup_metadata.R
│   ├── 01_download_gutenberg.R
│   ├── 02_clean_shakespeare_structure.R
│   ├── 03_validate_parsing.R
│   ├── 04_tokenise.R
│   ├── 05_filter.R
│   ├── 06_export_json.R
│   ├── 07_export_sqlite.R
│   ├── 08_frequency_analysis.R
│   ├── 09_relationship_analysis.R
│   ├── 10_interactive_networks.R
│   └── 11_sentiment_analysis.R
├── data/
│   ├── raw/                    # Downloaded Gutenberg texts
│   ├── cleaned/                # Structured CSV files
│   ├── processed/
│   │   ├── tokens/            # Tokenised data
│   │   └── filtered/          # Multiple filtered datasets
│   ├── json/                   # JSON exports
|   |   | full_text
│   │   ├── all_shakespeare.json
|   |   └── full_text/
│   │   |   └── *.json
│   ├── metadata/               # Metadata and logs
│   │   ├── meta_shakespeare.csv
│   │   ├── meta_shakespeare.json
│   │   ├── tokenisation_log.csv
│   │   ├── tokenisation_log.json
│   │   ├── unique_characters.csv
│   │   ├── character_statistics.csv
│   │   └── characters_per_play.csv
│   ├── networks/               # Metadata and logs
│   │   ├── network_metrics.csv
│   │   └── *.graphml
│   └── shakespeare.sqlite      # SQLite database
├── plots/
|   └── character_networks/
|   |   └── *.png
│   ├── frequency/              # Frequency analysis plots
|   |   └──by_genre             # Most frequent words by genre     
│   ├── relationships/          # Network and bigram plots
│   └── sentiment/              # Sentiment analysis plots
└── interactive_networks/       # Interactive HTML visualisations
|   └── characters/
│   |   ├── index.html
│   |   ├── *_files /            
│   |   └── *.html
|   └── correlations /
│   |   ├── index.html
│   |   ├── *_files / 
│   |   └── *.html
```

## Data Schema

### Cleaned Data Columns

-   `gutenberg_id`, `short_title`, `gutenberg_title`, `genre`, `year`, `author`
-   `period` (Elizabethan/Jacobean), `romance`, `problem_play`, `roman`
-   `section` (front_matter/contents)
-   `class` (front matter: title, author, contents, dramatis personae; contents: reference/directions/dialogue)
-   `act`, `scene`, `character (dialogue)`, `line_number (dialogue)`
-   `text`

### Token Data Columns

Same as cleaned data, with `word` replacing `text`

## Key Features

-   **Export Formats:** CSV, JSON, SQLite for different use cases
-   **Visualisations:** High-quality plots (600 DPI)

## Custom Stop Words

The pipeline uses an extended stop word list including standard English stop words plus Shakespearean-specific terms.

## Running the Pipeline

1.  **Setup:** Ensure all required packages are installed
2.  **Execute in order:** Run scripts 00-07 sequentially to build the complete dataset
3.  **Analysis:** Run scripts 08-11 independently for different analyses
4.  **Configuration:** Modify parameters at the top of each analysis script for different subsets

## Example Usage

### Analyse Elizabethan Tragedies

``` r
# In 09_relationship_analysis.R or 11_sentiment_analysis.R
FILTER_BY <- "period"
FILTER_VALUES <- c("Elizabethan")
```

### Query SQLite Database

``` r
library(DBI)
library(RSQLite)

con <- dbConnect(SQLite(), "data/shakespeare.sqlite")

# Get Hamlet's dialogue
dbGetQuery(con, "SELECT * FROM hamlet WHERE class = 'dialogue'")

# Character line counts
dbGetQuery(con, "SELECT * FROM character_line_counts WHERE short_title = 'Hamlet'")

dbDisconnect(con)
```

## Attribution and License

Scripts and concepts used in this project are adapted from *Tidy Text Mining with R* by Julia Silge & David Robinson (2017).\
This material is licensed under the [Creative Commons Attribution-NonCommercial-ShareAlike 3.0 United States License (CC BY-NC-SA 3.0 US)](https://creativecommons.org/licenses/by-nc-sa/3.0/us/).\
By using or adapting these scripts, proper credit is given to the original authors, and any modifications are shared under the same license.

## Citations

**Tidy Text Mining with R**\
**Description:** A comprehensive introduction to text mining in R using tidy data principles\
**Source:** Julia Silge & David Robinson (2017). [GitHub repository](https://github.com/juliasilge/tidy-text-mining). Code and book manuscript under CC BY-NC-SA license.

**text2map (R package)**\
**Description:** R tools for text matrices, embeddings, document-term matrices, and computational text analysis, including metadata (e.g., Shakespeare metadata) and functions for embedding-, frequency-, and network-based analysis\
**Source:** [text2map on GitLab](https://gitlab.com/culturalcartography/text2map) by Dustin Stoltz & Marshall A. Taylor (MIT License, 2022–)\
**DOI / Citation:** Stoltz, D. S., & Taylor, M. A. (2022). *text2map: R Tools for Text Matrices, Embeddings, and Networks*. Available at <https://gitlab.com/culturalcartography/text2map>

## See Also

[github.com/nrennie/shakespeare](https://github.com/nrennie/shakespeare/) contains data on the plays of William Shakespeare from [shakespeare.mit.edu](https://shakespeare.mit.edu/).

[github.com/dracor-org/shakedracor](https://github.com/dracor-org/shakedracor/) contains data and analysis on the plays of William Shakespeare from the [Folger Shakespeare Library](https://www.folgerdigitaltexts.org/).
