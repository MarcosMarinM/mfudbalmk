#!/usr/bin/env Rscript

# Function to escape non-ASCII characters in a string
escape_non_ascii <- function(text) {
  # We use charToRaw and then format as \uXXXX
  # This is a bit complex in R, but we can iterate through characters
  
  chars <- strsplit(text, "")[[1]]
  result <- sapply(chars, function(c) {
    if (utf8ToInt(c) > 127) {
      # Get hex code
      sprintf("\\u%04x", utf8ToInt(c))
    } else {
      c
    }
  })
  paste(result, collapse = "")
}

# Function to sanitize a whole file
sanitize_file <- function(filepath) {
  message(paste("Sanitizing:", filepath))
  lines <- readLines(filepath, warn = FALSE, encoding = "UTF-8")
  
  # Process each line
  sanitized_lines <- sapply(lines, escape_non_ascii)
  
  # Write back as ASCII (with escapes)
  writeLines(sanitized_lines, filepath, useBytes = TRUE)
}

# List of files to sanitize
files_to_sanitize <- c(
  "R/08_functions.R",
  "R/09_data_loading.R",
  "R/10_data_processing.R",
  "R/11_aggregated_datasets.R",
  "R/12_assets.R",
  "R/13_html_generation.R",
  "R/14_finalization.R",
  "buildhtml.R"
)

for (f in files_to_sanitize) {
  if (file.exists(f)) {
    sanitize_file(f)
  }
}
message("Bulk sanitization complete.")
