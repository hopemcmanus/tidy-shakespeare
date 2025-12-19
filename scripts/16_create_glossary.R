################################################################################
# 16_generate_glossary.R
# 
# Description: Generates glossary page matching main index.html style
#
################################################################################

library(tidyverse)
library(jsonlite)

# Configuration ----------------------------------------------------------------
OUTPUT_DIR <- "glossary"
OUTPUT_HTML <- file.path(OUTPUT_DIR, "index.html")
OUTPUT_CSV <- file.path(OUTPUT_DIR, "glossary.csv")
OUTPUT_JSON <- file.path(OUTPUT_DIR, "glossary.json")

# Create output directory
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

# Create glossary data with categories ----------------------------------------
glossary <- tribble(
  ~category, ~term, ~definition, ~reference, ~reference_text,
  
  # Tidy Principles
  "Tidy Principles", "Token", "A meaningful unit of text (typically a word) extracted during tokenisation. In this project, tokens include individual words from Shakespeare's dialogue, stage directions, and references.", "https://www.tidytextmining.com/tidytext.html#the-unnest_tokens-function", "Tokenisation",
  
  "Tidy Principles", "Tokenisation", "The process of breaking text into individual words or meaningful units. Uses unnest_tokens() to convert text data into one-token-per-row format.", "https://www.tidytextmining.com/tidytext.html#the-unnest_tokens-function", "Unnest Tokens Function",
  
  "Tidy Principles", "Stop Words", "Common words (e.g., 'the', 'a', 'and') that are often removed from analysis. This project uses extended stop words including Shakespearean terms like 'thou', 'thee', 'thy'.", "https://www.tidytextmining.com/tidytext.html#word-frequencies", "Word Frequencies",
  
  "Tidy Principles", "Tidy Text", "A table with one-token-per-row, where each row represents a single word along with its metadata (play, act, scene, character). Follows tidy data principles.", "https://www.tidytextmining.com/tidytext.html", "Tidy Text Format",
  
  "Tidy Principles", "Tidy Data", "Each variable forms a column, each observation forms a row, each type of observational unit forms a table. Applied to text: one-token-per-row format.", "https://www.tidytextmining.com/tidytext.html#contrasting-tidy-text-with-other-data-structures", "Tidy vs Non-Tidy Text",
  
  # Frequency Analysis
  "Frequency Analysis", "Term Frequency (TF)", "How frequently a word appears in a document. Calculated as the number of times a term appears divided by total terms in the document.", "https://www.tidytextmining.com/tfidf.html#term-frequency-in-jane-austens-novels", "Term Frequency",
  
  "Frequency Analysis", "Inverse Document Frequency (IDF)", "Decreases the weight for commonly used words and increases the weight for words that are not used very much. Calculated as log(total documents / documents containing term).", "https://www.tidytextmining.com/tfidf.html#zipfs-law", "Zipf's Law",
  
  "Frequency Analysis", "TF-IDF", "Term Frequency-Inverse Document Frequency. A numerical statistic that reflects how important a word is to a document in a collection. High TF-IDF means the word is frequent in this document but rare in others, making it distinctive.", "https://www.tidytextmining.com/tfidf.html", "TF-IDF",
  
  "Frequency Analysis", "Zipf's Law", "The observation that word frequency is inversely proportional to its rank. The most frequent word appears approximately twice as often as the second most frequent word, three times as often as the third, etc.", "https://www.tidytextmining.com/tfidf.html#zipfs-law", "Zipf's Law Explained",
  
  # N-grams and Relationships
  "N-grams and Relationships", "Bigram", "A sequence of two adjacent words. Used to examine word pairs and common phrases (e.g., 'good night', 'fair lady').", "https://www.tidytextmining.com/ngrams.html#tokenizing-by-n-gram", "N-gram Tokenisation",
  
  "N-grams and Relationships", "N-gram", "A contiguous sequence of n items (words) from text. Bigrams are 2-grams, trigrams are 3-grams, etc.", "https://www.tidytextmining.com/ngrams.html", "N-grams & Word Combinations",
  
  "N-grams and Relationships", "Correlation", "A measure of how often words appear together relative to how often they appear separately. Used to find word pairs that commonly co-occur.", "https://www.tidytextmining.com/ngrams.html#counting-and-correlating-pairs-of-words-with-the-widyr-package", "Pairwise Correlation",
  
  "N-grams and Relationships", "Pairwise Correlation", "Examining relationships between pairs of words to find which words tend to appear together in the same section or scene.", "https://www.tidytextmining.com/ngrams.html#pairwise-correlation", "Correlation Analysis",
  
  # Sentiment Analysis
  "Sentiment Analysis", "Sentiment Analysis", "Computational identification and categorisation of opinions in text to determine emotional tone (positive, negative, neutral).", "https://www.tidytextmining.com/sentiment.html", "Sentiment Analysis",
  
  "Sentiment Analysis", "Sentiment Lexicon", "A dictionary of words labelled with sentiment scores or categories. This project uses three: Bing (positive/negative), AFINN (numeric scores), and NRC (emotions).", "https://www.tidytextmining.com/sentiment.html#the-sentiments-datasets", "Sentiment Lexicons",
  
  "Sentiment Analysis", "Bing Lexicon", "A sentiment lexicon that categorises words into binary positive/negative sentiment. Created by Bing Liu and collaborators.", "https://www.tidytextmining.com/sentiment.html#the-sentiments-datasets", "Bing Sentiment Lexicon",
  
  "Sentiment Analysis", "AFINN Lexicon", "A sentiment lexicon that assigns words a score from -5 (most negative) to +5 (most positive). Created by Finn Årup Nielsen.", "https://www.tidytextmining.com/sentiment.html#the-sentiments-datasets", "AFINN Sentiment Lexicon",
  
  "Sentiment Analysis", "NRC Lexicon", "National Research Council Canada lexicon that categorises words into emotions (joy, fear, anger, etc.) and positive/negative sentiment.", "https://www.tidytextmining.com/sentiment.html#the-sentiments-datasets", "NRC Emotion Lexicon",
  
  # Network Analysis
  "Network Analysis", "Network Graph", "A visualisation showing relationships between entities (characters or words) as nodes connected by edges.", "https://www.tidytextmining.com/ngrams.html#visualizing-a-network-of-bigrams-with-ggraph", "Network Visualisation",
  
  "Network Analysis", "Node", "A point in a network graph representing an entity (e.g., a character or word).", "https://www.tidytextmining.com/ngrams.html#visualizing-a-network-of-bigrams-with-ggraph", "Bigram Networks",
  
  "Network Analysis", "Edge", "A connection between two nodes in a network graph, representing a relationship (e.g., characters appearing together or word co-occurrence).", "https://www.tidytextmining.com/ngrams.html#visualizing-a-network-of-bigrams-with-ggraph", "Network Edges",
  
  "Network Analysis", "Degree Centrality", "The number of direct connections a node has. High degree indicates a character/word that connects to many others.", "https://www.tidytextmining.com/ngrams.html", "Centrality Measures",
  
  "Network Analysis", "Betweenness Centrality", "Measures how often a node lies on the shortest path between other nodes. High betweenness indicates importance in connecting different parts of the network.", "https://www.tidytextmining.com/ngrams.html", "Network Centrality",
  
  # Data Structures
  "Data Structures", "Document-Term Matrix", "A matrix where rows represent documents (plays), columns represent terms (words), and values represent frequency or TF-IDF scores.", "https://www.tidytextmining.com/dtm.html", "Document-Term Matrices",
  
  # Project-Specific
  "Project-Specific", "Character Network", "A network where nodes represent characters and edges represent co-occurrence (appearing in the same scene). Edge weight indicates how many scenes they share.", "https://www.tidytextmining.com/ngrams.html#visualizing-a-network-of-bigrams-with-ggraph", "Character Networks",
  
  "Project-Specific", "Word Correlation Network", "A network showing which words tend to appear together in sections of text, with correlation strength determining edge weight.", "https://www.tidytextmining.com/ngrams.html#pairwise-correlation", "Word Correlations",
  
  "Project-Specific", "Act-Scene Structure", "Shakespeare's organisational system dividing plays into major sections (Acts) and smaller segments (Scenes). Used as document boundaries for some analyses.", "https://www.tidytextmining.com/", "Text Mining with R",
  
  "Project-Specific", "Dialogue vs Directions", "Two main text classes in plays: dialogue (spoken by characters) and stage directions (describing action). Analysed separately or together depending on research question.", "https://www.tidytextmining.com/", "Tidy Text Mining"
)

# Sort by category, then term
glossary <- glossary %>%
  arrange(category, term)

# Generate table rows
table_rows <- paste(
  apply(glossary, 1, function(row) {
    sprintf('                    <tr>
                        <td class="term">%s</td>
                        <td class="definition">%s</td>
                        <td class="category">%s</td>
                        <td class="reference"><a href="%s" target="_blank">%s</a></td>
                    </tr>',
            row["term"],
            row["definition"],
            row["category"],
            row["reference"],
            row["reference_text"])
  }),
  collapse = "\n"
)

# Generate HTML ----------------------------------------------------------------
html_content <- paste0('<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Glossary: Tidy Shakespeare</title>
    <style>
        * {
            margin: 0;
            padding: 0;
            box-sizing: border-box;
        }
      
      
        body {
            font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif;
            line-height: 1.6;
            color: #1a1a1a;
            background: #ffffff;
        }
        
        .header {
            background: #2c2c2c;
            color: #ffffff;
            padding: 1.25rem 2rem;
            border-bottom: 1px solid #e0e0e0;
            display: flex;
            justify-content: space-between;
            align-items: center;
        }
        
        .header-left h1 {
            font-size: 1.5rem;
            font-weight: 600;
            margin-bottom: 0.25rem;
        }
        
        .header-subtitle {
            font-size: 0.85rem;
            color: #d0d0d0;
        }
        
        .header-right {
            display: flex;
            gap: 1rem;
        }
        
        .header-link {
            color: #ffffff;
            text-decoration: none;
            font-size: 0.9rem;
            padding: 0.4rem 0.8rem;
            border: 1px solid #555;
            border-radius: 3px;
            transition: all 0.2s;
        }
        
        .header-link:hover {
            background: #444;
            border-color: #666;
        }
        
        .header-link.current {
            background: #444;
            border-color: #666;
            cursor: default;
        }
        
        .container {
            max-width: 1200px;
            margin: 0 auto;
            padding: 2rem;
        }
        
        .intro {
            background: #fafafa;
            padding: 1.5rem;
            border-left: 4px solid #333;
            margin-bottom: 2rem;
            border-radius: 4px;
        }
        
        .intro h2 {
            font-size: 1.3rem;
            margin-bottom: 0.75rem;
            color: #1a1a1a;
        }
        
        .intro p {
            color: #333;
            line-height: 1.8;
        }
        
        
        .data-table {
            width: 100%;
            border-collapse: collapse;
            font-size: 0.9rem;
        }
        
        .data-table thead {
            background: #f5f5f5;
            border-bottom: 2px solid #e0e0e0;
        }
        
        .data-table th {
            padding: 0.75rem 1rem;
            text-align: left;
            font-weight: 600;
            font-size: 0.8rem;
            text-transform: uppercase;
            letter-spacing: 0.5px;
            color: #666;
        }
        
        .data-table td {
            padding: 0.75rem 1rem;
            border-bottom: 1px solid #f0f0f0;
            vertical-align: top;
        }
        
        .data-table tbody tr:hover {
            background: #fafafa;
        }
        
        .category {
            color: #666;
            font-size: 0.85rem;
        }
        
        .term {
            font-weight: 500;
            color: #1a1a1a;
        }
        
        .definition {
            color: #333;
            line-height: 1.7;
        }
        
        .reference {
            text-align: center;
        }
        
        .footer {
            background: #f5f5f5;
            border-top: 1px solid #e0e0e0;
            padding: 0.75rem 2rem;
            font-size: 0.8rem;
            color: #666;
        }
        
        .footer a {
            color: #333;
            text-decoration: none;
        }
        
        .footer a:hover {
            text-decoration: underline;
        }
        
        .footer-content {
            max-width: 1200px;
            margin: 0 auto;
            display: flex;
            flex-wrap: wrap;
            gap: 0.5rem 1.5rem;
            align-items: center;
        }
        
        .hamburger {
            display: none;
            background: none;
            border: none;
            color: white;
            font-size: 1.5rem;
            cursor: pointer;
            padding: 0.5rem;
        }
        
        @media (max-width: 768px) {
            .header {
                position: relative;
            }
            
            .hamburger {
                display: block;
                position: absolute;
                right: 1rem;
                top: 50%;
                transform: translateY(-50%);
            }
            
             
            .header-right {
                position: fixed;
                top: 70px;
                right: 0;
                width: 200px;
                background: #2c2c2c;
                flex-direction: column;
                padding: 1rem;
                border-left: 1px solid #555;
                box-shadow: -2px 0 8px rgba(0,0,0,0.2);
                transform: translateX(100%);
                transition: transform 0.3s;
                z-index: 1000;
            }
            
            .header-right.active {
                transform: translateX(0);
            }
            
            .container {
                padding: 1rem;
            }
            
            .header h1 {
                font-size: 1.25rem;
            }
            
            .data-table {
                font-size: 0.85rem;
                margin: 0;
            }
            
            .data-table th,
            .data-table td {
                padding: 0.5rem;
            
     
            }
        }
    </style>
</head>
<body>
    <div class="header">
        <div class="header-left">
            <h1>Tidy Shakespeare</h1>
            <div class="header-subtitle">Tidy Data and Text Analysis of Shakespeare Plays</div>
        </div>
        <button class="hamburger" onclick="toggleMenu()">☰</button>
        <div class="header-right">
            <a href="../index.html" class="header-link">← Back to Main Page</a>
            <a href="../about/index.html" class="header-link">About</a>
            <a href="../glossary/index.html" class="header-link current">Glossary</a>
            <a href="https://github.com/hopemcmanus/tidy-shakespeare" target="_blank" class="header-link">GitHub</a>
        </div>
    </div>
    
    <div class="container">
        <div class="intro">
            <h2>About This Glossary</h2>
            <p>This glossary has a list of terms used in the Tidy Shakespeare project. Each term has a definition, a category and a reference, primarily <a href="https://github.com/juliasilge/tidy-text-mining" target="_blank"><em>Text Mining with R</em></a> by Julia Silge and David Robinson.</p>
        </div>
        
        <div class="table-wrapper">
            <table class="data-table">
                <thead>
                    <tr>
                        <th style="width: 20%">Term</th>
                        <th style="width: 50%">Definition</th>
                        <th style="width: 15%">Category</th>
                        <th style="width: 15%">Reference</th>
                    </tr>
                </thead>
                <tbody>
', table_rows, '
                </tbody>
            </table>
        </div>
    </div>
    </div>
    <div class="footer">
        <div class="footer-content">
            <span>Texts from <a href="https://www.gutenberg.org/" target="_blank">Project Gutenberg</a></span>
            <span><a href="https://github.com/hopemcmanus/tidy-shakespeare?tab=readme-ov-file#attribution-and-license" target="_blank">Code</a> is licensed under <a href="https://creativecommons.org/licenses/by-nc-sa/3.0/us/" target="_blank">CC BY-NC-SA 3.0</a></span>
            <span>Download Metadata: <a href="../data/metadata/meta_shakespeare.json" download>JSON</a> | <a href="../data/metadata/meta_shakespeare.csv" download>CSV</a></span>
            <span><a href="../contact/index.html">Contact</a></span>
        </div>
    </div>
    
    <script>
        function toggleMenu() {
            var menu = document.querySelector(".header-right");
            if (menu.classList.contains("active")) {
                menu.classList.remove("active");
            } else {
                menu.classList.add("active");
            }
        }
    </script>
</body>
</html>')

# Write files ------------------------------------------------------------------
writeLines(html_content, OUTPUT_HTML)
write_csv(glossary, OUTPUT_CSV)
write_json(glossary, OUTPUT_JSON, pretty = TRUE)

# Summary ----------------------------------------------------------------------
message("\n", strrep("=", 80))
message("GLOSSARY PAGE GENERATED")
message(strrep("=", 80))
message("\nCreated glossary with ", nrow(glossary), " terms across ", n_distinct(glossary$category), " categories")
message("\nOutput files:")
message("  HTML: ", OUTPUT_HTML)
message("  CSV:  ", OUTPUT_CSV)
message("  JSON: ", OUTPUT_JSON)
message("\nTo view: Open glossary/index.html in your web browser")
message(strrep("=", 80), "\n")

message("Terms by category:")
category_counts <- glossary %>%
  count(category) %>%
  arrange(desc(n))

for (i in 1:nrow(category_counts)) {
  message(sprintf("  %s: %d terms", category_counts$category[i], category_counts$n[i]))
}