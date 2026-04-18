#!/usr/bin/env Rscript

# Function to rename ALL instances of Cyrillic variables to ASCII
# This version is more aggressive.
sanitize_and_rename_v2 <- function(filepath) {
  message(paste("Sanitizing and Renaming (V2):", filepath))
  lines <- readLines(filepath, warn = FALSE, encoding = "UTF-8")
  text <- paste(lines, collapse = "\n")
  
  # 1. Broad replacements for common Cyrillic identifiers
  # Replace vid_natprevaruvanje in all forms
  text <- gsub("\u0432\u0438\u0434_\u043d\u0430\u0442\u043f\u0440\u0435\u0432\u0430\u0440\u0443\u0432\u0430\u045a\u0435", "vid_natprevaruvanje", text)
  text <- gsub("\u043d\u0430\u0442\u043f\u0440\u0435\u0432\u0430\u0440\u0443\u0432\u0430\u045a\u0435", "natprevaruvanje", text)
  text <- gsub("vid_natprevaruvanje", "vid_natprevaruvanje", text) # redundancy
  
  # Replace columns and variables
  text <- gsub("script_contrase\u00f1a", "script_password", text)
  text <- gsub("\u0418\u043c\u0435", "Ime", text)
  text <- gsub("\u0422\u0438\u043f", "Tip", text)
  text <- gsub("\u0431_\u0440", "b_r", text)
  
  # Also handle cases where they were already escaped by the previous script
  text <- gsub("\\\\u0432\\\\u0438\\\\u0434_\\\\u043d\\\\u0430\\\\u0442\\\\u043f\\\\u0440\\\\u0435\\\\u0432\\\\u0430\\\\u0440\\\\u0443\\\\u0432\\\\u0430\\\\u045a\\\\u0435", "vid_natprevaruvanje", text)
  text <- gsub("\\\\u043d\\\\u0430\\\\u0442\\\\u043f\\\\u0440\\\\u0435\\\\u0432\\\\u0430\\\\u0440\\\\u0443\\\\u0432\\\\u0430\\\\u045a\\\\u0435", "natprevaruvanje", text)
  text <- gsub("\\\\u0418\\\\u043c\\\\u0435", "Ime", text)
  text <- gsub("\\\\u0422\\\\u0438\\\\u043f", "Tip", text)
  text <- gsub("script_contrase\\\\u00f1a", "script_password", text)

  # 2. Nuclear option for identifiers: Find anything that looks like a variable name 
  # but contains \u and replace it with backticks if it's not in a string.
  # Instead of complex regex, let's just target the known ones.
  
  # 3. Final pass: ensure all non-ASCII are escaped in strings/comments
  chars <- strsplit(text, "")[[1]]
  result <- sapply(chars, function(c) {
    ic <- utf8ToInt(c)
    if (ic > 127) {
      # If it's literally a non-ascii char in the file now
      sprintf("\\u%04x", ic)
    } else {
      c
    }
  })
  
  new_content <- paste(result, collapse = "")
  writeLines(new_content, filepath, useBytes = TRUE)
}

files <- c(
  "R/08_functions.R",
  "R/09_data_loading.R",
  "R/10_data_processing.R",
  "R/11_aggregated_datasets.R",
  "R/12_assets.R",
  "R/13_html_generation.R",
  "R/14_finalization.R",
  "buildhtml.R"
)

for (f in files) {
  if (file.exists(f)) {
    sanitize_and_rename_v2(f)
  }
}
message("Renaming and Sanitization V2 complete.")
