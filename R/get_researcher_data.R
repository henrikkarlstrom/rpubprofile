#' Get Scopus researcher data
#'
#' Pass the function a valid Scopus researcher ID and a valid Scopus API key and it
#' returns a data frame of the recorded publications with relevant metadata.
#'
#' @param scopus_id string
#' @param apiKey string
#'
#' @return data frame
#' @export
#'
#' @examples
#' get_researcher_data(scopus_id = "23056907500", apiKey = Sys.getenv("SCOPUS_API_KEY"))
get_researcher_data <- function(scopus_id, apiKey) {
  
  if(!grepl("^[0-9]{1,}$", scopus_id)) {
    stop(
      "Invalid Scopus ID. Please input only numbers."
    )
  }

  # API query specification
  data <- httr::GET(
    url = paste0(
      "https://api.elsevier.com/content/search/scopus?query=AU-ID(",
      scopus_id,
      ")"
      ),
    query = list(
      apiKey = apiKey,
      count = 200,
      field = "prism:coverDate,dc:title,prism:publicationName,citedby-count,author"
      ),
    httr::add_headers(Accept = "application/json")
    )

  #stops the function if the call is not successful
  if(httr::status_code(data) != 200){
    stop(
      paste0(
        "API call failed with status code ",
        httr::status_code(data)
        )
      )
  }

  #parse the response
  data <- jsonlite::fromJSON(httr::content(data, "text"))

  #subset the returned data and return it as a data frame
  data <- data[["search-results"]][["entry"]]
  
  authors <- do.call(
    rbind, 
    data[["author"]]
    )

  data[["author_count"]] <- as.numeric(
    lapply(
      data[["author"]], 
      function(x) length(x[[1]])
      )
  )
  
  authors <- authors[authors[["authid"]] == scopus_id, ]
  
  data[["order"]] <- authors[["@seq"]]
  
  data[["first_or_last"]] = as.factor(
    ifelse(data[["author_count"]] == data[["order"]],
           1, 
           ifelse(data[["order"]] == 1,
                  1,
                  0)
    )
  )
    
  
  
  data[["author"]] <- paste(authors[["given-name"]], authors[["surname"]])
  
  data <- data[, c(3:8, 10)]
  
  names(data) <- c(
    "Title", 
    "Venue", 
    "Year", 
    "Citations",
    "Author",
    "author_count", 
    "first_or_last"
    )
  
  data[["Year"]] <- as.numeric(
    substr(data[["Year"]], 1, 4)
  )
  
  data[["Citations"]] <- as.numeric(data[["Citations"]])

  return(data)
}
