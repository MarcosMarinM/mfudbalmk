library(httr)
library(stringr)

check_id <- function(id) {
  url <- sprintf("https://www.ffm.mk/delegiranje/%s/", id)
  res <- GET(url, add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36"))
  if (status_code(res) == 200) {
    cont <- content(res, as = "text")
    token <- str_match(cont, "ss:\\s*['\"]([^'\"\\s]+)['\"]")[, 2]
    cat("ID", id, "Token:", if(!is.na(token)) token else "NOT FOUND", "\n")
    if (is.na(token)) {
       # Print a bit of the source where ss might be
       pos <- str_locate(cont, "ss:")
       if (!is.na(pos[1])) {
         cat("Snippet near ss: ", substr(cont, pos[1], pos[1]+50), "\n")
       } else {
         cat("String 'ss:' not found in source\n")
       }
    }
  } else {
    cat("ID", id, "Status:", status_code(res), "\n")
  }
}

check_id("9586697")
check_id("9942607")
