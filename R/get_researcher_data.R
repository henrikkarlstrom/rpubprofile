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
                                 field = "prism:coverDate,author,dc:creator,citedby-count,prism:issn"),
                    httr::add_headers(Accept = "application/json"))

  #stops the function if the call is not successful
  if(httr::status_code(data) != 200){
    stop(paste0("API call failed with status code ", httr::status_code(data)))
  }

  #parse the response
  data <- jsonlite::fromJSON(httr::content(data, "text"))

  #subset the returned data and return it as a data frame
  data <- data[["search-results"]][["entry"]]

  return(data)
}
