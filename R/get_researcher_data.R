#' Get Scopus researcher data
#'
#' Pass the function a valid Scopus researcher ID and a valid Scopus API key and
#' it returns a list containing a data frame of the recorded publications with 
#' relevant metadata.
#'
#' @param scopus_id string
#' @param apiKey string
#'
#' @return list containing publication data frame and collaboration network
#' nodes and edges
#' 
#' @export
#'
#' @examples
#' get_researcher_data(
#' scopus_id = "23056907500", 
#' apiKey = Sys.getenv("SCOPUS_API_KEY")
#' )
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
  
  #get author name for network graphing
  author <- data[["author"]][[1]][["authname"]][
    which(
      data[["author"]][[1]][["authid"]] == scopus_id
    )
    ]
  
  #calculate author collabs for each publication
  authcombo <- lapply(
    data[["author"]], 
    function(x) if(nrow(x) == 1){
      NA
    } else{
      data.frame(
        t(combn(x[["authname"]], 2)),
        stringsAsFactors = FALSE
      ) 
    }
  )
  
  #weight collabs according to number of co-authors on each publication
  authcombo <- lapply(
    authcombo,
    function(x) cbind(
      x, 
      width = 2 / nrow(x)
      )
  )
  
  #remove single-author publications from the collaboration network
  authcombo <- Filter(
    function(x) length(x) > 1, 
    authcombo
    )
  
  #define edges of the collaboration network
  edges <- do.call(
    rbind, 
    authcombo
    )
  colnames(edges) <- c("from", "to", "width")
  edges <- edges[edges[["from"]] == author | edges[["to"]] == author, ]
  edges <- edges[edges[["width"]] > 0.1, ]
  
  #define the collaborators
  nodes <- data.frame(
    id = unique(c(edges[["from"]], edges[["to"]]))
  )
  
  nodes[["group"]] <- ifelse(nodes[["id"]] == author, "main", "co-pub")
  nodes[["label"]] <- nodes[["id"]]
  nodes[["title"]] <- nodes[["id"]]
  nodes[["value"]] <- ifelse(nodes[["id"]] == author, 25, 15)
  
  #create a list object to return from the function
  network <- list(edges, nodes, author)
  
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

  return(list(data, network))
}
