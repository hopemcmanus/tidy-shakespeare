################################################################################
# 15b_patch_html_app.R
# 
# Description: Patches the existing index.html with the following changes:
#   1. Sidebar play items show title only (no metadata)
#   2. Full Text section moved before analyses in navigation
#   3. Project Resources moved under master table
#   4. Correlations displayed vertically (not side-by-side)
#
################################################################################

library(stringr)

# Read existing index.html
html_file <- "index.html"

if (!file.exists(html_file)) {
  stop("ERROR: index.html not found. Run 15_generate_static_html_app.R first.")
}

message("Reading index.html...")
html <- readLines(html_file, warn = FALSE)
html <- paste(html, collapse = "\n")

message("Applying patches...\n")

# PATCH 1: Simplify sidebar play items (title only)
message("1. Simplifying sidebar play items...")
html <- str_replace(
  html,
  fixed("div.innerHTML = `
                    <div class=\"play-item-title\">${play.short_title}</div>
                    <div class=\"play-item-meta\">
                        <span>${play.year || \"—\"}</span>
                        <span>${play.genre}</span>
                        <span>${(play.tokens || 0).toLocaleString()} tokens</span>
                    </div>
                `;"),
  "div.innerHTML = `
                    <div class=\"play-item-title\">${play.short_title}</div>
                `;"
)

# PATCH 2: Reorder navigation links (Full Text before analyses)
message("2. Reordering navigation links...")
old_nav <- '<a href="#overview" class="nav-link" onclick="scrollToSection(event, \'overview\')">Overview</a>
                    <a href="#character-network" class="nav-link" onclick="scrollToSection(event, \'character-network\')">Character Network</a>
                    <a href="#full-text" class="nav-link" onclick="scrollToSection(event, \'full-text\')">Full Text</a>'

new_nav <- '<a href="#overview" class="nav-link" onclick="scrollToSection(event, \'overview\')">Overview</a>
                    <a href="#full-text" class="nav-link" onclick="scrollToSection(event, \'full-text\')">Full Text</a>
                    <a href="#character-network" class="nav-link" onclick="scrollToSection(event, \'character-network\')">Character Network</a>'

html <- str_replace(html, fixed(old_nav), new_nav)

# PATCH 3: Reorder sections in HTML (Full Text before Character Network)
message("3. Reordering content sections...")

# This is complex, so we'll use a marker-based approach
old_sections <- '<!-- Overview Section -->
                <section class="section" id="overview">
                    <h2 class="section-title">Overview</h2>
                    <div class="info-grid" id="overview-stats">
                        <!-- Populated by JavaScript -->
                    </div>
                </section>
                
                <!-- Character Network Section -->
                <section class="section" id="character-network">
                    <h2 class="section-title">Character Network</h2>
                    <div id="character-network-content">
                        <!-- Populated by JavaScript -->
                    </div>
                </section>
                
                <!-- Full Text Section -->
                <section class="section" id="full-text">
                    <h2 class="section-title">Full Text</h2>
                    <div class="info-grid" id="fulltext-stats">
                        <!-- Populated by JavaScript -->
                    </div>
                    <h3 class="section-subtitle">Preview (First 50 rows)</h3>
                    <div class="data-preview">
                        <table class="data-table" id="fulltext-table">
                            <!-- Populated by JavaScript -->
                        </table>
                    </div>
                </section>'

new_sections <- '<!-- Overview Section -->
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
                    <h3 class="section-subtitle">Preview (First 50 rows)</h3>
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
                </section>'

html <- str_replace(html, fixed(old_sections), new_sections)

# PATCH 4: Add Project Resources under master table
message("4. Adding Project Resources under master table...")

old_master_table_end <- '</table>
                </div>
            </div>
            
            <!-- Play Detail View -->'

new_master_table_end <- '</table>
                </div>
                
                <div style="margin-top: 2rem; padding-top: 1.5rem; border-top: 1px solid #e0e0e0;">
                    <h3 style="font-size: 1.1rem; font-weight: 600; margin-bottom: 1rem;">Project Resources</h3>
                    <div class="download-buttons">
                        <a href="data/metadata/meta_shakespeare.csv" class="download-btn" download>All Plays Metadata (CSV)</a>
                        <a href="data/metadata/meta_shakespeare.json" class="download-btn" download>All Plays Metadata (JSON)</a>
                        <a href="data/metadata/unique_characters.csv" class="download-btn" download>Unique Characters</a>
                        <a href="data/metadata/character_statistics.csv" class="download-btn" download>Character Statistics</a>
                    </div>
                </div>
            </div>
            
            <!-- Play Detail View -->'

html <- str_replace(html, fixed(old_master_table_end), new_master_table_end)

# PATCH 5: Stack correlations vertically (remove plot-grid wrapper)
message("5. Modifying correlations to display vertically...")

# Change plot-grid to vertical stacking
old_corr_grid <- 'content.innerHTML = found ? `<div class="plot-grid">${html}</div>`'
new_corr_vert <- 'content.innerHTML = found ? `<div style="display: flex; flex-direction: column; gap: 1.5rem;">${html}</div>`'

html <- str_replace(html, fixed(old_corr_grid), new_corr_vert)

# Write patched HTML
message("\nWriting patched index.html...")
writeLines(html, html_file)

message("\n================================================================================")
message("SUCCESS: index.html has been patched")
message("================================================================================")
message("\nChanges applied:")
message("  ✓ Sidebar play items now show title only")
message("  ✓ Full Text section moved before Character Network")
message("  ✓ Project Resources added under master table")
message("  ✓ Correlations now display vertically")
message("\nRefresh your browser to see the changes.")
message("================================================================================\n")