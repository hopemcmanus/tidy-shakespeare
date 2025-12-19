################################################################################
# 17_generate_contact.R
# 
# Description: Generates contact page with Formspree form matching main index.html style
#
################################################################################

library(tidyverse)

# Configuration ----------------------------------------------------------------
OUTPUT_DIR <- "about"
OUTPUT_HTML <- file.path(OUTPUT_DIR, "index.html")

# Create output directory
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

# Generate HTML ----------------------------------------------------------------
html_content <- paste0('<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>About Tidy Shakespeare</title>
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
    display: flex;
    flex-direction: column;
    min-height: 100vh;
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
            max-width: 1200px;
            padding: 2rem;
            min-height: calc(100vh - 200px);
            flex: 1;
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
        
        .form-wrapper {
            background: #fafafa;
            padding: 2rem;
            border: 1px solid #e0e0e0;
            border-radius: 4px;
        }
        
        .form-group {
            margin-bottom: 1.5rem;
        }
        
        label {
            display: block;
            font-weight: 600;
            font-size: 0.9rem;
            color: #333;
            margin-bottom: 0.5rem;
        }
        
        input[type="text"],
        input[type="email"],
        textarea {
            width: 100%;
            padding: 0.75rem;
            border: 1px solid #ddd;
            border-radius: 3px;
            font-size: 0.9rem;
            font-family: inherit;
            transition: border-color 0.2s;
        }
        
        input[type="text"]:focus,
        input[type="email"]:focus,
        textarea:focus {
            outline: none;
            border-color: #333;
        }
        
        textarea {
            resize: vertical;
            min-height: 150px;
        }
        
        .submit-btn {
            background: #333;
            color: white;
            border: none;
            padding: 0.75rem 2rem;
            border-radius: 3px;
            cursor: pointer;
            font-size: 0.9rem;
            font-weight: 600;
            transition: background 0.2s;
        }
        
        .submit-btn:hover {
            background: #1a1a1a;
        }
        
        .submit-btn:disabled {
            background: #999;
            cursor: not-allowed;
        }
        
        .form-status {
            margin-top: 1rem;
            padding: 1rem;
            border-radius: 3px;
            display: none;
        }
        
        .form-status.success {
            background: #e8f5e9;
            color: #2e7d32;
            border: 1px solid #4caf50;
        }
        
        .form-status.error {
            background: #ffebee;
            color: #c62828;
            border: 1px solid #f44336;
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
        <h1>Tidy Shakespeare</h1>
        <div class="header-subtitle">Tidy Data and Text Analysis of Shakespeare Plays</div>
    </div>
    <button class="hamburger" onclick="toggleMenu()">☰</button>
    <div class="header-right" id="header-menu">
        <a href="../about/index.html" class="header-link">About</a>
        <a href="../glossary/index.html" class="header-link">Glossary</a>
        <a href="https://github.com/hopemcmanus/tidy-shakespeare" target="_blank" class="header-link">GitHub</a>
    </div>
</div>
    
    <div class="container">
        <div class="intro">
            <h2>About Tidy Shakespeare</h2>
            <p>A project applying tidy data principles to structure and analyse Shakespeare plays</p>
        </div>
        <div id="readme-content" class="readme-wrapper">
    <p>Loading README...</p>
</div>
            </div>


    
    <div class="footer">
        <div class="footer-content">
         <span>Texts from <a href="https://www.gutenberg.org/" target="_blank">Project Gutenberg</a></span>
          <span><a href="https://github.com/hopemcmanus/tidy-shakespeare?tab=readme-ov-file#attribution-and-license" target="_blank">Code</a> licensed under <a href="https://creativecommons.org/licenses/by-nc-sa/3.0/us/" target="_blank">CC BY-NC-SA 3.0</a></span>
          <span>Download Metadata: <a href="data/metadata/meta_shakespeare.json" download>JSON</a> | <a href="data/metadata/meta_shakespeare.csv" download>CSV</a></span>
          <span><a href="../contact/index.html"">Contact</a></span>
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
   
    async function loadReadme() {
        try {
            const response = await fetch("https://raw.githubusercontent.com/hopemcmanus/tidy-shakespeare/main/README.md");
            const markdown = await response.text();
            
            // Extract only specific sections
            const sections = extractSections(markdown, [
                "Description",
                "Data",
                "Key Features"
            ]);
            
            const html = sections
                .replace(/^### (.*$)/gim, "<h3>$1</h3>")
                .replace(/^## (.*$)/gim, "<h2>$1</h2>")
                .replace(/^# (.*$)/gim, "<h1>$1</h1>")
                .replace(/\\*\\*(.*?)\\*\\*/gim, "<strong>$1</strong>")
                .replace(/\\*(.*?)\\*/gim, "<em>$1</em>")
                .replace(/\\n/gim, "<br>");
            
            document.getElementById("readme-content").innerHTML = html;
        } catch (error) {
            document.getElementById("readme-content").innerHTML = "<p>Error loading README</p>";
        }
    }
    
    function extractSections(markdown, sectionTitles) {
        let result = "";
        const lines = markdown.split("\\n");
        let capturing = false;
        let currentLevel = 0;
        
        for (let i = 0; i < lines.length; i++) {
            const line = lines[i];
            
            // Check if this is a header line
            const headerMatch = line.match(/^(#{1,6})\\s+(.+)$/);
            
            if (headerMatch) {
                const level = headerMatch[1].length;
                const title = headerMatch[2];
                
                // Check if this is one of our desired sections
if (sectionTitles.includes(title)) {
        capturing = true;
                    currentLevel = level;
                    result += line + "\\n";
                }
                // Stop capturing if we hit a same-level or higher-level header
                else if (capturing && level <= currentLevel) {
                    capturing = false;
                }
                else if (capturing) {
                    result += line + "\\n";
                }
            }
            else if (capturing) {
                result += line + "\\n";
            }
        }
        
        return result;
    }
    
    loadReadme();
</script>
</body>
</html>')
    

# Write file -------------------------------------------------------------------
writeLines(html_content, OUTPUT_HTML)

# Summary ----------------------------------------------------------------------
message("\n", strrep("=", 80))
message("CONTACT PAGE GENERATED")
message(strrep("=", 80))
message("\nOutput file: ", OUTPUT_HTML)
message("\nTo view: Open about/index.html in your web browser")
message(strrep("=", 80), "\n")