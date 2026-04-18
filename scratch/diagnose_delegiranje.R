library(httr)
library(rvest)
library(stringr)
library(jsonlite)
library(dplyr)
library(purrr)

# Match ID to test
match_id <- "9942607"

# 1. Test token extraction
obtener_ss_token <- function(match_id) {
  url <- sprintf("https://www.ffm.mk/delegiranje/%s/", match_id)
  cat("Fetching URL:", url, "\n")
  res <- tryCatch(
    GET(url, add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36")),
    error = function(e) { cat("Error in GET:", e$message, "\n"); return(NULL) }
  )
  
  if (is.null(res)) return(NULL)
  cat("Status code:", status_code(res), "\n")
  if (status_code(res) != 200) return(NULL)
  
  cont <- content(res, as = "text", encoding = "UTF-8")
  # Look for ss token
  token <- str_match(cont, "ss:\\s*['\"]([^'\"\\s]+)['\"]")[, 2]
  return(token)
}

token <- obtener_ss_token(match_id)
cat("Token found:", if(!is.na(token)) token else "NONE", "\n")

# 2. Test AJAX call
if (!is.na(token) && !is.null(token)) {
  url_ajax <- "https://www.ffm.mk/wp-admin/admin-ajax.php"
  
  test_status <- function(status_val) {
    cat("\nTesting status:", status_val, "\n")
    body <- list(
      action = "igalcom_get_officials",
      match_id = as.character(match_id),
      status = status_val,
      ss = token
    )
    
    res <- POST(url_ajax, body = body, encode = "form", 
                add_headers("X-Requested-With" = "XMLHttpRequest",
                            "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36"), 
                timeout(15))
    
    cat("AJAX Status code:", status_code(res), "\n")
    if (status_code(res) == 200) {
      cont <- content(res, as = "text", encoding = "UTF-8")
      cat("Raw response snippet:", substr(cont, 1, 100), "...\n")
      json_data <- tryCatch(fromJSON(cont), error = function(e) { cat("JSON error:", e$message, "\n"); NULL })
      
      if (!is.null(json_data)) {
        cat("JSON Success:", json_data$success, "\n")
        if (isTRUE(json_data$success) && !is.null(json_data$data) && json_data$data != "") {
          html_snippet <- read_html(json_data$data)
          nodos <- html_snippet %>% html_elements(".ffm-deleg__official")
          cat("Number of officials found:", length(nodos), "\n")
          if (length(nodos) > 0) {
            # Print first official
            rol_raw <- nodos[[1]] %>% html_element(".ffm-deleg__official-role") %>% html_text(trim = TRUE)
            nombre <- nodos[[1]] %>% html_element(".ffm-deleg__official-name") %>% html_text(trim = TRUE)
            cat("Example official (raw rol): '", rol_raw, "'\n", sep="")
            cat("Example official (hex rol): ", paste(charToRaw(rol_raw), collapse=" "), "\n")
            cat("Example official (nombre): '", nombre, "'\n", sep="")
          }
          return(TRUE)
        } else {
          cat("No data in JSON response\n")
        }
      }
    }
    return(FALSE)
  }
  
  test_status("FINISHED")
  test_status("PLAYED")
  test_status("SCHEDULED")
}
