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
  author <- lapply(data[[2]], 
                   function(x) httr::GET(url = x[["@href"]][[2]], 
                      query = list(apiKey = apiKey)))
  
  author <- lapply(author, 
                   function(x) jsonlite::fromJSON(httr::content(x, "text")))
  
  #extracting author name values
  author <- lapply(author, 
                   function(x) x[["abstracts-retrieval-response"]][["authors"]][["author"]])
  
  profile_name <- author[[1]][which(author[[1]][["@auid"]] == scopus_id),][["preferred-name"]][["ce:indexed-name"]]
  
  #create table of results
  data <- data.frame(Year = as.numeric(substr(data[["prism:coverDate"]], 1, 4)),
                     Citations = as.numeric(data[["citedby-count"]]),
                     Venue = data[["prism:publicationName"]],
                     DOI = data[["prism:doi"]],
                     Author = profile_name,
                     author_count = unlist(lapply(author, function(x) length(x[["@auid"]]))),
                     order = unlist(lapply(author, function(x) which(x[["@auid"]] == scopus_id))))

  return(data)
}
