# Verification script for delegation fix
source("scrape.R")

# We want to test get_delegiranje_data without a token (to test auto-acquisition)
test_id <- "9942607"
cat("Testing get_delegiranje_data for ID:", test_id, "without providing a token...\n")

del_data <- get_delegiranje_data(test_id, ss_token = NULL)

if (!is.null(del_data) && nrow(del_data) > 0) {
  cat("SUCCESS: Extracted", nrow(del_data), "officials.\n")
  print(head(del_data))
} else {
  cat("FAILURE: Could not extract delegation data.\n")
}
