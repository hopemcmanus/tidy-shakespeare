################################################################################
# 15_generate_static_html_app.R
# 
# Description: Generates a single-page static HTML application for exploring
#              Shakespeare corpus with two-level sidebar navigation and minimal
#              color scheme
#
# Updates:
#   - Fixed token counts display
#   - Fixed tags display (romance, roman, problem_play)
#   - Reorganized sidebar (plays list above filters)
#   - Removed token slider
#   - Added attribution footer with links
#   - Improved full text section (search, filters, removed gutenberg_title)
#   - Correlations display vertically (interactive first)
#   - Removed duplicate project resources from play pages
#
################################################################################

library(tidyverse)
library(jsonlite)
library(DBI)
library(RSQLite)

# Configuration ----------------------------------------------------------------
METADATA_DIR <- "data/metadata"
JSON_DIR <- "data/json/full_text"
CLEANED_DIR <- "data/cleaned"
TOKENS_DIR <- "data/processed/tokens"
PLOTS_DIR <- "plots"
OUTPUT_FILE <- "index.html"

# GitHub info for attribution
GITHUB_REPO <- "https://github.com/hopemcmanus/tidy-shakespeare"
PORTFOLIO_URL <- "https://hopemcmanus.github.io/portfolio/shakespeare/"

# Helper Functions -------------------------------------------------------------

#' Generate HTML string with embedded JSON data
generate_html <- function(plays_json, token_json) {
  
  html <- paste0('<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Shakespeare Corpus Explorer</title>
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
        
        .container {
            display: flex;
            height: calc(100vh - 140px);
            overflow: hidden;
        }
        
        .sidebar {
            width: 320px;
            background: #fafafa;
            border-right: 1px solid #e0e0e0;
            display: flex;
            flex-direction: column;
            overflow: hidden;
        }
        
        /* Level 1: Master navigation */
        .master-nav {
            display: flex;
            flex-direction: column;
            height: 100%;
            overflow: hidden;
        }
        
        .master-nav.hidden {
            display: none;
        }
        
        .plays-list {
            flex: 1;
            overflow-y: auto;
            padding: 0.5rem;
            border-bottom: 1px solid #e0e0e0;
        }
        
        .plays-count {
            font-size: 0.8rem;
            color: #666;
            margin-bottom: 0.5rem;
            padding: 0 0.5rem;
            font-weight: 600;
        }
        
        .play-item {
            padding: 0.6rem 0.75rem;
            margin-bottom: 0.25rem;
            cursor: pointer;
            border-radius: 3px;
            border: 1px solid transparent;
            transition: all 0.2s;
        }
        
        .play-item:hover {
            background: #f0f0f0;
            border-color: #e0e0e0;
        }
        
        .play-item.selected {
            background: #e8e8e8;
            border-color: #ccc;
        }
        
        .play-item-title {
            font-weight: 500;
            font-size: 0.9rem;
            color: #1a1a1a;
        }
        
        .filters {
            padding: 1rem;
            background: #f5f5f5;
        }
        
        .filter-section {
            margin-bottom: 1rem;
        }
        
        .filter-section:last-child {
            margin-bottom: 0;
        }
        
        .filter-label {
            font-weight: 600;
            font-size: 0.75rem;
            text-transform: uppercase;
            letter-spacing: 0.5px;
            color: #666;
            margin-bottom: 0.4rem;
            display: block;
        }
        
        .checkbox-group {
            display: flex;
            flex-wrap: wrap;
            gap: 0.5rem;
        }
        
        .checkbox-item {
            display: flex;
            align-items: center;
        }
        
        .checkbox-item input {
            margin-right: 0.3rem;
            cursor: pointer;
        }
        
        .checkbox-item label {
            font-size: 0.85rem;
            cursor: pointer;
            user-select: none;
            color: #333;
        }
        
        .clear-filters {
            background: #333;
            color: white;
            border: none;
            padding: 0.4rem 0.8rem;
            border-radius: 3px;
            cursor: pointer;
            font-size: 0.85rem;
            margin-top: 0.75rem;
            width: 100%;
            transition: background 0.2s;
        }
        
        .clear-filters:hover {
            background: #1a1a1a;
        }
        
        /* Level 2: Play navigation */
        .play-nav {
            display: none;
            flex-direction: column;
            height: 100%;
            overflow: hidden;
        }
        
        .play-nav.active {
            display: flex;
        }
        
        .play-nav-header {
            padding: 1rem;
            border-bottom: 1px solid #e0e0e0;
            background: #f5f5f5;
        }
        
        .back-button {
            background: none;
            border: none;
            color: #333;
            cursor: pointer;
            font-size: 0.85rem;
            padding: 0.4rem 0;
            margin-bottom: 0.75rem;
            display: flex;
            align-items: center;
            transition: color 0.2s;
        }
        
        .back-button:hover {
            color: #000;
        }
        
        .back-button::before {
            content: "← ";
            margin-right: 0.5rem;
        }
        
        .play-nav-title {
            font-size: 1.1rem;
            font-weight: 600;
            color: #1a1a1a;
            margin-bottom: 0.25rem;
        }
        
        .play-nav-meta {
            font-size: 0.8rem;
            color: #666;
        }
        
        .play-nav-links {
            flex: 1;
            overflow-y: auto;
            padding: 0.5rem;
        }
        
        .nav-link {
            display: block;
            padding: 0.6rem 0.75rem;
            margin-bottom: 0.25rem;
            color: #333;
            text-decoration: none;
            border-radius: 3px;
            font-size: 0.85rem;
            transition: all 0.2s;
            border-left: 3px solid transparent;
        }
        
        .nav-link:hover {
            background: #f0f0f0;
        }
        
        .nav-link.active {
            background: #e8e8e8;
            border-left-color: #333;
            font-weight: 500;
        }
        
        .main-content {
            flex: 1;
            overflow-y: auto;
            background: #fff;
        }
        
        /* Master table view */
        .master-table-view {
            padding: 2rem;
        }
        
        .master-table-view.hidden {
            display: none;
        }
        
        .master-table-view h2 {
            font-size: 1.5rem;
            font-weight: 600;
            color: #1a1a1a;
            margin-bottom: 1.5rem;
        }
        
        .table-wrapper {
            overflow-x: auto;
            border: 1px solid #e0e0e0;
            border-radius: 4px;
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
        }
        
        .data-table tbody tr {
            cursor: pointer;
            transition: background 0.2s;
        }
        
        .data-table tbody tr:hover {
            background: #fafafa;
        }
        
        /* Play detail view */
        .play-detail-view {
            display: none;
            padding: 2rem;
        }
        
        .play-detail-view.active {
            display: block;
        }
        
        .section {
            margin-bottom: 3rem;
            scroll-margin-top: 1rem;
        }
        
        .section-title {
            font-size: 1.3rem;
            font-weight: 600;
            color: #1a1a1a;
            margin-bottom: 1rem;
            padding-bottom: 0.5rem;
            border-bottom: 2px solid #e0e0e0;
        }
        
        .section-subtitle {
            font-size: 1rem;
            font-weight: 500;
            color: #333;
            margin-bottom: 0.75rem;
            margin-top: 1.5rem;
        }
        
        .info-grid {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(180px, 1fr));
            gap: 1rem;
            margin-bottom: 1.5rem;
        }
        
        .info-card {
            padding: 1rem;
            background: #fafafa;
            border-radius: 4px;
            border: 1px solid #e0e0e0;
        }
        
        .info-card-label {
            font-size: 0.75rem;
            color: #666;
            text-transform: uppercase;
            letter-spacing: 0.5px;
            margin-bottom: 0.4rem;
        }
        
        .info-card-value {
            font-size: 1.2rem;
            font-weight: 600;
            color: #1a1a1a;
        }
        
        .download-buttons {
            display: flex;
            gap: 0.75rem;
            flex-wrap: wrap;
            margin-bottom: 1.5rem;
        }
        
        .download-btn {
            display: inline-flex;
            align-items: center;
            padding: 0.5rem 1rem;
            background: #333;
            color: white;
            text-decoration: none;
            border-radius: 3px;
            font-size: 0.85rem;
            font-weight: 500;
            transition: background 0.2s;
        }
        
        .download-btn:hover {
            background: #1a1a1a;
        }
        
        .download-btn::before {
            content: "↓ ";
            margin-right: 0.4rem;
        }
        
        .plot-item {
            border: 1px solid #e0e0e0;
            border-radius: 4px;
            overflow: hidden;
            background: #fafafa;
            margin-bottom: 1.5rem;
        }
        
        .plot-item h4 {
            padding: 0.75rem 1rem;
            background: #f5f5f5;
            font-size: 0.85rem;
            font-weight: 600;
            color: #333;
            border-bottom: 1px solid #e0e0e0;
        }
        
        .plot-item img {
            width: 100%;
            display: block;
            background: white;
        }
        
        .plot-item iframe {
            width: 100%;
            height: 500px;
            border: none;
            display: block;
            background: white;
        }
        
        .data-preview {
            max-height: 500px;
            overflow: auto;
            border: 1px solid #e0e0e0;
            border-radius: 4px;
        }
        
        .data-preview .data-table {
            border: none;
        }
        
        .fulltext-controls {
            display: flex;
            gap: 1rem;
            margin-bottom: 1rem;
            flex-wrap: wrap;
        }
        
        .search-box {
            flex: 1;
            min-width: 200px;
            padding: 0.5rem 0.75rem;
            border: 1px solid #ddd;
            border-radius: 3px;
            font-size: 0.9rem;
        }
        
        .filter-select {
            padding: 0.5rem 0.75rem;
            border: 1px solid #ddd;
            border-radius: 3px;
            font-size: 0.9rem;
            background: white;
            cursor: pointer;
        }
        
        .no-data {
            padding: 2rem;
            text-align: center;
            color: #999;
            font-style: italic;
            background: #fafafa;
            border: 1px solid #e0e0e0;
            border-radius: 4px;
        }
        
        .footer {
            background: #f5f5f5;
            border-top: 1px solid #e0e0e0;
            padding: 1rem 2rem;
            font-size: 0.85rem;
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
        }
        
        .footer-line {
            margin-bottom: 0.5rem;
        }
        
        @media (max-width: 1024px) {
            .sidebar {
                width: 280px;
            }
        }
        
        @media (max-width: 768px) {
            .container {
                flex-direction: column;
            }
            
            .sidebar {
                width: 100%;
                height: 40vh;
            }
            
            .main-content {
                height: 60vh;
            }
        }
    </style>
</head>
<body>
    <div class="header">
        <div class="header-left">
            <h1>Shakespeare Corpus Explorer</h1>
            <div class="header-subtitle">37 plays from Project Gutenberg · Tidy text analysis pipeline</div>
        </div>
        <div class="header-right">
            <a href="', PORTFOLIO_URL, '" class="header-link" target="_blank">About</a>
            <a href="', GITHUB_REPO, '" class="header-link" target="_blank">GitHub</a>
        </div>
    </div>
    
    <div class="container">
        <!-- SIDEBAR -->
        <div class="sidebar">
            <!-- Level 1: Master Navigation -->
            <div class="master-nav" id="master-nav">
                <div class="plays-list">
                    <div class="plays-count" id="plays-count">Loading...</div>
                    <div id="plays-list-items">
                        <!-- Populated by JavaScript -->
                    </div>
                </div>
                
                <div class="filters">
                    <!-- Period Filter -->
                    <div class="filter-section">
                        <span class="filter-label">Period</span>
                        <div class="checkbox-group">
                            <div class="checkbox-item">
                                <input type="checkbox" id="filter-elizabethan" value="Elizabethan" checked>
                                <label for="filter-elizabethan">Elizabethan</label>
                            </div>
                            <div class="checkbox-item">
                                <input type="checkbox" id="filter-jacobean" value="Jacobean" checked>
                                <label for="filter-jacobean">Jacobean</label>
                            </div>
                        </div>
                    </div>
                    
                    <!-- Genre Filter -->
                    <div class="filter-section">
                        <span class="filter-label">Genre</span>
                        <div class="checkbox-group">
                            <div class="checkbox-item">
                                <input type="checkbox" id="filter-tragedy" value="Tragedy" checked>
                                <label for="filter-tragedy">Tragedy</label>
                            </div>
                            <div class="checkbox-item">
                                <input type="checkbox" id="filter-comedy" value="Comedy" checked>
                                <label for="filter-comedy">Comedy</label>
                            </div>
                            <div class="checkbox-item">
                                <input type="checkbox" id="filter-history" value="History" checked>
                                <label for="filter-history">History</label>
                            </div>
                        </div>
                    </div>
                    
                    <!-- Tags Filter -->
                    <div class="filter-section">
                        <span class="filter-label">Tags</span>
                        <div class="checkbox-group">
                            <div class="checkbox-item">
                                <input type="checkbox" id="filter-romance" value="romance" checked>
                                <label for="filter-romance">Romance</label>
                            </div>
                            <div class="checkbox-item">
                                <input type="checkbox" id="filter-roman" value="roman" checked>
                                <label for="filter-roman">Roman</label>
                            </div>
                            <div class="checkbox-item">
                                <input type="checkbox" id="filter-problem" value="problem_play" checked>
                                <label for="filter-problem">Problem</label>
                            </div>
                        </div>
                    </div>
                    
                    <button class="clear-filters" onclick="clearFilters()">Clear Filters</button>
                </div>
            </div>
            
            <!-- Level 2: Play Navigation -->
            <div class="play-nav" id="play-nav">
                <div class="play-nav-header">
                    <button class="back-button" onclick="backToMasterView()">Back to All Plays</button>
                    <div class="play-nav-title" id="play-nav-title">Play Title</div>
                    <div class="play-nav-meta" id="play-nav-meta">Year · Genre · Period</div>
                </div>
                <div class="play-nav-links" id="play-nav-links">
                    <a href="#overview" class="nav-link" onclick="scrollToSection(event, \'overview\')">Overview</a>
                    <a href="#full-text" class="nav-link" onclick="scrollToSection(event, \'full-text\')">Full Text</a>
                    <a href="#character-network" class="nav-link" onclick="scrollToSection(event, \'character-network\')">Character Network</a>
                    <a href="#frequency" class="nav-link" onclick="scrollToSection(event, \'frequency\')">Frequency Analysis</a>
                    <a href="#sentiment" class="nav-link" onclick="scrollToSection(event, \'sentiment\')">Sentiment Analysis</a>
                    <a href="#bigrams" class="nav-link" onclick="scrollToSection(event, \'bigrams\')">Bigram Networks</a>
                    <a href="#correlations" class="nav-link" onclick="scrollToSection(event, \'correlations\')">Word Correlations</a>
                    <a href="#downloads" class="nav-link" onclick="scrollToSection(event, \'downloads\')">Downloads</a>
                </div>
            </div>
        </div>
        
        <!-- MAIN CONTENT -->
        <div class="main-content" id="main-content">
            <!-- Master Table View -->
            <div class="master-table-view" id="master-table-view">
                <h2>Shakespeare Plays</h2>
                <div class="table-wrapper">
                    <table class="data-table">
                        <thead>
                            <tr>
                                <th>Gutenberg ID</th>
                                <th>Title</th>
                                <th>Year</th>
                                <th>Genre</th>
                                <th>Period</th>
                                <th>Tokens</th>
                            </tr>
                        </thead>
                        <tbody id="master-table-body">
                            <!-- Populated by JavaScript -->
                        </tbody>
                    </table>
                </div>
            </div>
            
            <!-- Play Detail View -->
            <div class="play-detail-view" id="play-detail-view">
                <!-- Overview Section -->
                <section class="section" id="overview">
                    <h2 class="section-title">Overview</h2>
                    <div class="info-grid" id="overview-stats">
                        <!-- Populated by JavaScript -->
                    </div>
                </section>
                
                <!-- Full Text Section -->
                <section class="section" id="full-text">
                    <h2 class="section-title">Full Text</h2>
                    <div class="info-grid" id="fulltext-stats">
                        <!-- Populated by JavaScript -->
                    </div>
                    
                    <h3 class="section-subtitle">Download Options</h3>
                    <div class="download-buttons" id="download-fulltext">
                        <!-- Populated by JavaScript -->
                    </div>
                    
                    <h3 class="section-subtitle">Preview with Search & Filters</h3>
                    <div class="fulltext-controls">
                        <input type="text" id="fulltext-search" class="search-box" placeholder="Search text...">
                        <select id="fulltext-class-filter" class="filter-select">
                            <option value="all">All Classes</option>
                            <option value="dialogue">Dialogue Only</option>
                            <option value="directions">Stage Directions Only</option>
                            <option value="references">References Only</option>
                        </select>
                    </div>
                    <div class="data-preview">
                        <table class="data-table" id="fulltext-table">
                            <!-- Populated by JavaScript -->
                        </table>
                    </div>
                </section>
                
                <!-- Character Network Section -->
                <section class="section" id="character-network">
                    <h2 class="section-title">Character Network</h2>
                    <div id="character-network-content">
                        <!-- Populated by JavaScript -->
                    </div>
                </section>
                
                <!-- Frequency Analysis Section -->
                <section class="section" id="frequency">
                    <h2 class="section-title">Frequency Analysis</h2>
                    <div id="frequency-content">
                        <!-- Populated by JavaScript -->
                    </div>
                </section>
                
                <!-- Sentiment Analysis Section -->
                <section class="section" id="sentiment">
                    <h2 class="section-title">Sentiment Analysis</h2>
                    <div id="sentiment-content">
                        <!-- Populated by JavaScript -->
                    </div>
                </section>
                
                <!-- Bigram Networks Section -->
                <section class="section" id="bigrams">
                    <h2 class="section-title">Bigram Networks</h2>
                    <div id="bigrams-content">
                        <!-- Populated by JavaScript -->
                    </div>
                </section>
                
                <!-- Word Correlations Section -->
                <section class="section" id="correlations">
                    <h2 class="section-title">Word Correlations</h2>
                    <div id="correlations-content">
                        <!-- Populated by JavaScript -->
                    </div>
                </section>
                
                <!-- Downloads Section -->
                <section class="section" id="downloads">
                    <h2 class="section-title">Downloads</h2>
                    <div id="downloads-content">
                        <!-- Populated by JavaScript -->
                    </div>
                </section>
            </div>
        </div>
    </div>
    
    <div class="footer">
        <div class="footer-content">
            <div class="footer-line">
                Derived from the <a href="https://www.gutenberg.org/" target="_blank">Project Gutenberg</a>. 
                Enhancements documented in our <a href="', GITHUB_REPO, '" target="_blank">README at GitHub</a>.
            </div>
            <div class="footer-line">
                Corpus licensed under <a href="https://creativecommons.org/licenses/by-nc-sa/3.0/us/" target="_blank">CC BY-NC-SA 3.0</a>
            </div>
            <div class="footer-line">
                Download a comprehensive table with metadata on all plays in the corpus: 
                <a href="data/metadata/meta_shakespeare.json" download>JSON</a> | 
                <a href="data/metadata/meta_shakespeare.csv" download>CSV</a>
            </div>
        </div>
    </div>
    
    <script>
        // Embedded data
        const EMBEDDED_PLAYS = ', plays_json, ';
        const EMBEDDED_TOKENS = ', token_json, ';
        
        // Global state
        let allPlays = [];
        let filteredPlays = [];
        let currentPlay = null;
        let currentFullTextData = [];
        
        // Initialize
        document.addEventListener("DOMContentLoaded", function() {
            console.log("Initializing Shakespeare Corpus Explorer...");
            loadMetadata();
            setupEventListeners();
            filterAndRenderPlays();
        });
        
        // Load metadata from embedded data
        function loadMetadata() {
            try {
                console.log("Loading embedded metadata...");
                
                if (!EMBEDDED_PLAYS || EMBEDDED_PLAYS.length === 0) {
                    throw new Error("No plays data embedded");
                }
                
                allPlays = EMBEDDED_PLAYS;
                
                // Merge token counts
                if (EMBEDDED_TOKENS && EMBEDDED_TOKENS.length > 0) {
                    allPlays = allPlays.map(play => {
                        const tokenInfo = EMBEDDED_TOKENS.find(t => t.gutenberg_id === play.gutenberg_id);
                        return {
                            ...play,
                            tokens: tokenInfo ? tokenInfo.tokens : 0
                        };
                    });
                }
                
                console.log(`Loaded ${allPlays.length} plays`);
                
            } catch (error) {
                console.error("Error loading metadata:", error);
                document.getElementById("plays-count").textContent = "Error: " + error.message;
            }
        }
        
        // Setup event listeners
        function setupEventListeners() {
            document.getElementById("filter-elizabethan").addEventListener("change", filterAndRenderPlays);
            document.getElementById("filter-jacobean").addEventListener("change", filterAndRenderPlays);
            document.getElementById("filter-tragedy").addEventListener("change", filterAndRenderPlays);
            document.getElementById("filter-comedy").addEventListener("change", filterAndRenderPlays);
            document.getElementById("filter-history").addEventListener("change", filterAndRenderPlays);
            document.getElementById("filter-romance").addEventListener("change", filterAndRenderPlays);
            document.getElementById("filter-roman").addEventListener("change", filterAndRenderPlays);
            document.getElementById("filter-problem").addEventListener("change", filterAndRenderPlays);
            
            // Scroll spy for nav links
            document.getElementById("main-content").addEventListener("scroll", updateActiveNavLink);
        }
        
        // Filter and render plays
        function filterAndRenderPlays() {
            const periods = [];
            if (document.getElementById("filter-elizabethan").checked) periods.push("Elizabethan");
            if (document.getElementById("filter-jacobean").checked) periods.push("Jacobean");
            
            const genres = [];
            if (document.getElementById("filter-tragedy").checked) genres.push("Tragedy");
            if (document.getElementById("filter-comedy").checked) genres.push("Comedy");
            if (document.getElementById("filter-history").checked) genres.push("History");
            
            filteredPlays = allPlays.filter(play => {
                if (periods.length > 0 && !periods.includes(play.period)) return false;
                if (genres.length > 0 && !genres.includes(play.genre)) return false;
                
                const romanceChecked = document.getElementById("filter-romance").checked;
                const romanChecked = document.getElementById("filter-roman").checked;
                const problemChecked = document.getElementById("filter-problem").checked;
                
                if (!romanceChecked && play.romance === "TRUE") return false;
                if (!romanChecked && play.roman === "TRUE") return false;
                if (!problemChecked && play.problem_play === "TRUE") return false;
                
                return true;
            });
            
            filteredPlays.sort((a, b) => (a.year || 0) - (b.year || 0));
            
            document.getElementById("plays-count").textContent = `${filteredPlays.length} of ${allPlays.length} plays`;
            
            renderPlaysList();
            renderMasterTable();
        }
        
        // Render plays list in sidebar
        function renderPlaysList() {
            const container = document.getElementById("plays-list-items");
            container.innerHTML = "";
            
            filteredPlays.forEach(play => {
                const div = document.createElement("div");
                div.className = "play-item";
                if (currentPlay && currentPlay.gutenberg_id === play.gutenberg_id) {
                    div.classList.add("selected");
                }
                div.onclick = () => loadPlayDetail(play);
                
                div.innerHTML = `
                    <div class="play-item-title">${play.short_title}</div>
                `;
                
                container.appendChild(div);
            });
        }
        
        // Render master table
        function renderMasterTable() {
            const tbody = document.getElementById("master-table-body");
            tbody.innerHTML = "";
            
            filteredPlays.forEach(play => {
                const row = document.createElement("tr");
                row.onclick = () => loadPlayDetail(play);
                
                row.innerHTML = `
                    <td>${play.gutenberg_id}</td>
                    <td><strong>${play.short_title}</strong></td>
                    <td>${play.year || "—"}</td>
                    <td>${play.genre}</td>
                    <td>${play.period || "—"}</td>
                    <td>${(play.tokens || 0).toLocaleString()}</td>
                `;
                
                tbody.appendChild(row);
            });
        }
        
        // Clear filters
        function clearFilters() {
            document.getElementById("filter-elizabethan").checked = true;
            document.getElementById("filter-jacobean").checked = true;
            document.getElementById("filter-tragedy").checked = true;
            document.getElementById("filter-comedy").checked = true;
            document.getElementById("filter-history").checked = true;
            document.getElementById("filter-romance").checked = true;
            document.getElementById("filter-roman").checked = true;
            document.getElementById("filter-problem").checked = true;
            filterAndRenderPlays();
        }
        
        // Load play detail
        async function loadPlayDetail(play) {
            currentPlay = play;
            
            // Update selected state in sidebar
            document.querySelectorAll(".play-item").forEach(item => item.classList.remove("selected"));
            event.target.closest(".play-item")?.classList.add("selected");
            
            // Switch to play navigation
            document.getElementById("master-nav").classList.add("hidden");
            document.getElementById("play-nav").classList.add("active");
            
            // Update play nav header
            document.getElementById("play-nav-title").textContent = play.short_title;
            document.getElementById("play-nav-meta").textContent = 
                `${play.year || "Year unknown"} · ${play.genre} · ${play.period || "Period unknown"}`;
            
            // Switch to play detail view
            document.getElementById("master-table-view").classList.add("hidden");
            document.getElementById("play-detail-view").classList.add("active");
            
            // Scroll to top
            document.getElementById("main-content").scrollTop = 0;
            
            // Load content
            await loadPlayContent(play);
        }
        
        // Back to master view
        function backToMasterView() {
            currentPlay = null;
            
            document.getElementById("master-nav").classList.remove("hidden");
            document.getElementById("play-nav").classList.remove("active");
            document.getElementById("master-table-view").classList.remove("hidden");
            document.getElementById("play-detail-view").classList.remove("active");
            
            document.getElementById("main-content").scrollTop = 0;
        }
        
        // Scroll to section
        function scrollToSection(event, sectionId) {
            event.preventDefault();
            const section = document.getElementById(sectionId);
            const mainContent = document.getElementById("main-content");
            
            const offsetTop = section.offsetTop - mainContent.offsetTop;
            mainContent.scrollTo({
                top: offsetTop,
                behavior: "smooth"
            });
        }
        
        // Update active nav link based on scroll position
        function updateActiveNavLink() {
            const mainContent = document.getElementById("main-content");
            const sections = document.querySelectorAll(".section");
            const navLinks = document.querySelectorAll(".nav-link");
            
            let currentSection = "";
            
            sections.forEach(section => {
                const sectionTop = section.offsetTop - mainContent.offsetTop;
                const sectionHeight = section.offsetHeight;
                const scrollPos = mainContent.scrollTop;
                
                if (scrollPos >= sectionTop - 100 && scrollPos < sectionTop + sectionHeight - 100) {
                    currentSection = section.id;
                }
            });
            
            navLinks.forEach(link => {
                link.classList.remove("active");
                if (link.getAttribute("href") === "#" + currentSection) {
                    link.classList.add("active");
                }
            });
        }
        
        // Load play content
        async function loadPlayContent(play) {
            const shortTitle = play.short_title.toLowerCase().replace(/[^a-z0-9]+/g, "_").replace(/^_+|_+$/g, "");
            
            // Overview
            loadOverview(play);
            
            // Full Text
            await loadFullText(play, shortTitle);
            
            // Character Network
            await loadCharacterNetwork(shortTitle);
            
            // Frequency Analysis
            await loadFrequencyAnalysis(shortTitle);
            
            // Sentiment Analysis
            await loadSentimentAnalysis(shortTitle);
            
            // Bigram Networks
            await loadBigramNetworks(shortTitle);
            
            // Word Correlations
            await loadWordCorrelations(shortTitle);
            
            // Downloads
            loadDownloads(play, shortTitle);
        }
        
        // Load overview
        function loadOverview(play) {
            const tags = [];
            if (play.romance === "TRUE") tags.push("Romance");
            if (play.roman === "TRUE") tags.push("Roman");
            if (play.problem_play === "TRUE") tags.push("Problem Play");
            
            document.getElementById("overview-stats").innerHTML = `
                <div class="info-card">
                    <div class="info-card-label">Gutenberg ID</div>
                    <div class="info-card-value">${play.gutenberg_id}</div>
                </div>
                <div class="info-card">
                    <div class="info-card-label">Year</div>
                    <div class="info-card-value">${play.year || "Unknown"}</div>
                </div>
                <div class="info-card">
                    <div class="info-card-label">Genre</div>
                    <div class="info-card-value" style="font-size: 1rem;">${play.genre}</div>
                </div>
                <div class="info-card">
                    <div class="info-card-label">Period</div>
                    <div class="info-card-value" style="font-size: 1rem;">${play.period || "Unknown"}</div>
                </div>
                <div class="info-card">
                    <div class="info-card-label">Total Tokens</div>
                    <div class="info-card-value">${(play.tokens || 0).toLocaleString()}</div>
                </div>
                <div class="info-card">
                    <div class="info-card-label">Tags</div>
                    <div class="info-card-value" style="font-size: 0.85rem;">${tags.length > 0 ? tags.join(", ") : "None"}</div>
                </div>
            `;
        }
        
        // Load full text
        async function loadFullText(play, shortTitle) {
            try {
                const response = await fetch(`data/json/full_text/${shortTitle}.json`);
                if (response.ok) {
                    const data = await response.json();
                    currentFullTextData = data;
                    
                    const dialogueCount = data.filter(row => row.class === "dialogue").length;
                    const directionCount = data.filter(row => row.class === "directions").length;
                    const characters = [...new Set(data.filter(row => row.character).map(row => row.character))];
                    
                    document.getElementById("fulltext-stats").innerHTML = `
                        <div class="info-card">
                            <div class="info-card-label">Total Lines</div>
                            <div class="info-card-value">${data.length.toLocaleString()}</div>
                        </div>
                        <div class="info-card">
                            <div class="info-card-label">Dialogue Lines</div>
                            <div class="info-card-value">${dialogueCount.toLocaleString()}</div>
                        </div>
                        <div class="info-card">
                            <div class="info-card-label">Stage Directions</div>
                            <div class="info-card-value">${directionCount.toLocaleString()}</div>
                        </div>
                        <div class="info-card">
                            <div class="info-card-label">Characters</div>
                            <div class="info-card-value">${characters.length}</div>
                        </div>
                    `;
                    
                    // Download buttons
                    document.getElementById("download-fulltext").innerHTML = `
                        <a href="data/cleaned/${shortTitle}.csv" class="download-btn" download>Cleaned Text (CSV)</a>
                        <a href="data/json/full_text/${shortTitle}.json" class="download-btn" download>Full Text (JSON)</a>
                    `;
                    
                    // Setup search and filter listeners
                    document.getElementById("fulltext-search").addEventListener("input", filterFullText);
                    document.getElementById("fulltext-class-filter").addEventListener("change", filterFullText);
                    
                    // Initial render
                    filterFullText();
                } else {
                    document.getElementById("fulltext-stats").innerHTML = \'<div class="no-data">Data not available</div>\';
                }
            } catch (error) {
                console.error("Error loading full text:", error);
                document.getElementById("fulltext-stats").innerHTML = \'<div class="no-data">Error loading data</div>\';
            }
        }
        
        // Filter full text based on search and class
        function filterFullText() {
            const searchTerm = document.getElementById("fulltext-search").value.toLowerCase();
            const classFilter = document.getElementById("fulltext-class-filter").value;
            
            let filtered = currentFullTextData;
            
            // Apply class filter
            if (classFilter !== "all") {
                filtered = filtered.filter(row => row.class === classFilter);
            }
            
            // Apply search filter
            if (searchTerm) {
                filtered = filtered.filter(row => 
                    (row.text && row.text.toLowerCase().includes(searchTerm)) ||
                    (row.character && row.character.toLowerCase().includes(searchTerm))
                );
            }
            
            // Limit to first 100 rows for performance
            renderFullTextTable(filtered.slice(0, 100));
        }
        
        // Render full text table (without gutenberg_title column)
        function renderFullTextTable(data) {
            const table = document.getElementById("fulltext-table");
            
            if (!data || data.length === 0) {
                table.innerHTML = \'<tr><td colspan="10" class="no-data">No matching rows</td></tr>\';
                return;
            }
            
            // Define columns to display (exclude gutenberg_title)
            const displayColumns = ["gutenberg_id", "short_title", "genre", "year", "author", "section", "class", "act", "scene", "character", "line_number", "text"];
            
            let html = "<thead><tr>";
            displayColumns.forEach(col => {
                html += `<th>${col.replace(/_/g, " ")}</th>`;
            });
            html += "</tr></thead><tbody>";
            
            data.forEach(row => {
                html += "<tr>";
                displayColumns.forEach(col => {
                    const value = row[col] || "";
                    html += `<td>${value}</td>`;
                });
                html += "</tr>";
            });
            
            html += "</tbody>";
            table.innerHTML = html;
        }
        
        // Load character network
        async function loadCharacterNetwork(shortTitle) {
            const content = document.getElementById("character-network-content");
            let html = "";
            let found = false;
            
            const staticPlot = `plots/character_networks/${shortTitle}_character_network.png`;
            if (await fileExists(staticPlot)) {
                html += `
                    <div class="plot-item">
                        <h4>Static Network</h4>
                        <img src="${staticPlot}" alt="Character Network">
                    </div>
                `;
                found = true;
            }
            
            const interactivePlot = `interactive_networks/characters/${shortTitle}.html`;
            if (await fileExists(interactivePlot)) {
                html += `
                    <div class="plot-item">
                        <h4>Interactive Network</h4>
                        <iframe src="${interactivePlot}"></iframe>
                    </div>
                `;
                found = true;
            }
            
            content.innerHTML = found ? html : \'<div class="no-data">No character network visualizations available</div>\';
        }
        
        // Load frequency analysis
        async function loadFrequencyAnalysis(shortTitle) {
            const content = document.getElementById("frequency-content");
            const freqPlot = `plots/frequency/individual_plays/${shortTitle}_top_words.png`;
            
            if (await fileExists(freqPlot)) {
                content.innerHTML = `
                    <div class="plot-item">
                        <h4>Most Frequent Words</h4>
                        <img src="${freqPlot}" alt="Word Frequency">
                    </div>
                `;
            } else {
                content.innerHTML = \'<div class="no-data">No frequency analysis available</div>\';
            }
        }
        
        // Load sentiment analysis
        async function loadSentimentAnalysis(shortTitle) {
            const content = document.getElementById("sentiment-content");
            const sentimentPlot = `plots/sentiment/individual_plays/sentiment_${shortTitle}_all_lexicons.png`;
            
            if (await fileExists(sentimentPlot)) {
                content.innerHTML = `
                    <div class="plot-item">
                        <h4>Sentiment by Act and Scene (All Lexicons)</h4>
                        <img src="${sentimentPlot}" alt="Sentiment Analysis">
                    </div>
                `;
            } else {
                content.innerHTML = \'<div class="no-data">No sentiment analysis available</div>\';
            }
        }
        
        // Load bigram networks
        async function loadBigramNetworks(shortTitle) {
            const content = document.getElementById("bigrams-content");
            const bigramPlot = `plots/relationships/networks/bigram_network_${shortTitle}.png`;
            
            if (await fileExists(bigramPlot)) {
                content.innerHTML = `
                    <div class="plot-item">
                        <h4>Bigram Network</h4>
                        <img src="${bigramPlot}" alt="Bigram Network">
                    </div>
                `;
            } else {
                content.innerHTML = \'<div class="no-data">No bigram network available</div>\';
            }
        }
        
        // Load word correlations (interactive first, then static)
        async function loadWordCorrelations(shortTitle) {
            const content = document.getElementById("correlations-content");
            let html = "";
            let found = false;
            
            // Interactive first
            const corrNetwork = `interactive_networks/correlations/${shortTitle}.html`;
            if (await fileExists(corrNetwork)) {
                html += `
                    <div class="plot-item">
                        <h4>Interactive Correlation Network</h4>
                        <iframe src="${corrNetwork}"></iframe>
                    </div>
                `;
                found = true;
            }
            
            // Static second
            const corrPlot = `plots/relationships/correlations/section_correlations_${shortTitle}.png`;
            if (await fileExists(corrPlot)) {
                html += `
                    <div class="plot-item">
                        <h4>Static Correlation Plot</h4>
                        <img src="${corrPlot}" alt="Word Correlations">
                    </div>
                `;
                found = true;
            }
            
            content.innerHTML = found ? html : \'<div class="no-data">No word correlation visualizations available</div>\';
        }
        
        // Load downloads
        function loadDownloads(play, shortTitle) {
            document.getElementById("downloads-content").innerHTML = `
                <h3 class="section-subtitle">Play Data Files</h3>
                <div class="download-buttons">
                    <a href="data/processed/tokens/${shortTitle}_tokens.csv" class="download-btn" download>Tokens (CSV)</a>
                </div>
            `;
        }
        
        // Helper: Check if file exists
        async function fileExists(path) {
            try {
                const response = await fetch(path, { method: "HEAD" });
                return response.ok;
            } catch {
                return false;
            }
        }
    </script>
</body>
</html>');
  
  return(html)
}

# Main Execution Function -----------------------------------------------------

main <- function() {
  message("================================================================================")
  message("Shakespeare Corpus Explorer - Static HTML Generator")
  message("================================================================================\n")
  
  # Validate required directories exist
  message("Validating project structure...")
  
  required_dirs <- c(
    METADATA_DIR,
    JSON_DIR,
    CLEANED_DIR,
    TOKENS_DIR,
    PLOTS_DIR
  )
  
  missing_dirs <- required_dirs[!dir.exists(required_dirs)]
  
  if (length(missing_dirs) > 0) {
    warning("Missing directories:\n  ", paste(missing_dirs, collapse = "\n  "))
    message("\nSome features may not work correctly without these directories.")
  } else {
    message("✓ All required directories found")
  }
  
  # Check for required metadata files
  message("\nChecking for required metadata files...")
  
  meta_csv <- file.path(METADATA_DIR, "meta_shakespeare.csv")
  meta_json <- file.path(METADATA_DIR, "meta_shakespeare.json")
  
  if (!file.exists(meta_csv) && !file.exists(meta_json)) {
    stop("ERROR: Neither meta_shakespeare.csv nor meta_shakespeare.json found in ", METADATA_DIR)
  }
  
  # Read metadata
  message("  Reading metadata...")
  if (file.exists(meta_json)) {
    plays_data <- read_json(meta_json)
    message("  ✓ Loaded meta_shakespeare.json")
  } else {
    plays_data <- read_csv(meta_csv, show_col_types = FALSE)
    message("  ✓ Loaded meta_shakespeare.csv")
    # Also save as JSON for future use
    write_json(plays_data, meta_json, pretty = TRUE)
    message("  ✓ Created meta_shakespeare.json")
  }
  
  message(sprintf("  Found %d plays", nrow(plays_data)))
  
  # Check for tokenisation log
  token_log_csv <- file.path(METADATA_DIR, "tokenisation_log.csv")
  token_log_json <- file.path(METADATA_DIR, "tokenisation_log.json")
  
  token_data <- list()
  
  if (file.exists(token_log_json)) {
    token_data <- read_json(token_log_json)
    message("  ✓ Loaded tokenisation_log.json")
  } else if (file.exists(token_log_csv)) {
    token_df <- read_csv(token_log_csv, show_col_types = FALSE)
    token_data <- token_df
    write_json(token_df, token_log_json, pretty = TRUE)
    message("  ✓ Loaded tokenisation_log.csv and created JSON")
  } else {
    message("  ℹ No tokenisation log found (optional)")
  }
  
  # Convert data to JSON strings for embedding
  message("\nPreparing embedded data...")
  plays_json <- toJSON(plays_data, auto_unbox = TRUE, pretty = FALSE)
  token_json <- toJSON(token_data, auto_unbox = TRUE, pretty = FALSE)
  
  # Generate HTML with embedded data
  message("\nGenerating index.html with embedded data...")
  html_content <- generate_html(plays_json, token_json)
  
  # Write to file
  writeLines(html_content, OUTPUT_FILE)
  
  message("\n================================================================================")
  message("SUCCESS: Static HTML application generated")
  message("================================================================================")
  message("\nOutput file: ", OUTPUT_FILE)
  message("\nFeatures:")
  message("  • Two-level sidebar navigation (master → play detail)")
  message("  • Plays list above filters in sidebar")
  message("  • Filters for period, genre, and tags (no token slider)")
  message("  • Master table view of all plays")
  message("  • Individual play pages with scrollable sections")
  message("  • Full text with search and class filters")
  message("  • Word correlations displayed vertically (interactive first)")
  message("  • Attribution footer with GitHub and portfolio links")
  message("  • Minimal black/grey/white color scheme")
  message("  • Embedded JSON data (works without web server)")
  message("\nTo view the application:")
  message("  1. Run a local server: servr::httd() or python -m http.server 8000")
  message("  2. Open http://localhost:8000 in your browser")
  message("\n================================================================================\n")
}

# Execute Main Function --------------------------------------------------------
if (!interactive()) {
  main()
} else {
  message("Script loaded. Run main() to generate the HTML application.")
}