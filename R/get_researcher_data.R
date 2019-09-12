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

  # API query specification
  data <- httr::GET(url = paste0("https://api.elsevier.com/content/search/scopus?query=AU-ID(",
                                 scopus_id,
                                 ")"),
                    query = list(apiKey = apiKey,
                                 count = 200,
                                 view = "STANDARD"),
                    httr::add_headers(Accept = "application/json"))

  #stops the function if the call is not successful
  if(httr::status_code(data) != 200){
    stop(paste0("API call failed with status code ", httr::status_code(data)))
  }

  #parse the response
  data <- jsonlite::fromJSON(httr::content(data, "text"))

  #subset the returned data and return it as a data frame
  data <- data[["search-results"]][["entry"]]
  
  #look up author indexed name using Scopus ID
  author <- httr::GET(url = data[[2]][[1]][["@href"]][[2]], 
                      query = list(apiKey = apiKey))
  
  author <- jsonlite::fromJSON(httr::content(author, "text"))
  
  #extrcating author name value
  author <- author[["abstracts-retrieval-response"]][["authors"]][["author"]]
  
  author <- author[which(author[["@auid"]] == scopus_id),][["preferred-name"]][["ce:indexed-name"]]
  
  #check if author is corresponding
  data[["corresponding"]] <- ifelse(data[["dc:creator"]] == author, 1, 0)
  
  #create table of results
  data <- data.frame(Year = as.Date(data[["prism:coverDate"]]),
                     Citations = as.numeric(data[["citedby-count"]]),
                     ISSN = data[["prism:issn"]],
                     Corresponding = as.numeric(data[["corresponding"]]),
                     Author = author)
  
  #convert date format to year values
  data[["Year"]] <- as.numeric(substr(data[["Year"]], 1, 4))

  return(data)
}
