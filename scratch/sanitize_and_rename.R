#!/usr/bin/env Rscript

# Function to rename variables and escape non-ASCII
sanitize_and_rename <- function(filepath) {
  message(paste("Sanitizing and Renaming:", filepath))
  lines <- readLines(filepath, warn = FALSE, encoding = "UTF-8")
  
  # 1. Renames
  text <- paste(lines, collapse = "\n")
  
  # Variable renames
  text <- gsub("script_contraseña", "script_password", text)
  text <- gsub("script_contrase\\\\u00f1a", "script_password", text)
  text <- gsub("вид_натпреварување", "vid_natprevaruvanje", text)
  text <- gsub("\\\\u0432\\\\u0438\\\\u0434_\\\\u043d\\\\u0430\\\\u0442\\\\u043f\\\\u0440\\\\u0435\\\\u0432\\\\u0430\\\\u0440\\\\u0443\\\\u0432\\\\u0430\\\\u045a\\\\u0435", "vid_natprevaruvanje", text)
  text <- gsub("Име", "Ime", text)
  text <- gsub("\\\\u0418\\\\u043c\\\\u0435", "Ime", text)
  text <- gsub("Тип", "Tip", text)
  text <- gsub("\\\\u0422\\\\u0438\\\\u043f", "Tip", text)
  
  # 2. Escape any remaining non-ASCII
  chars <- strsplit(text, "")[[1]]
  result <- sapply(chars, function(c) {
    if (utf8ToInt(c) > 127) {
      sprintf("\\u%04x", utf8ToInt(c))
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
    sanitize_and_rename(f)
  }
}
message("Renaming and Sanitization complete.")
