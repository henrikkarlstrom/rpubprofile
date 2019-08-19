#' Create a publication record data frame for easy plotting
#'
#' @param data dataframe
#' @param scopus_id string
#'
#' @return data frame with year (numeric), number of citations (numeric), corresponding
#' status (logical) and author name (string)
#' @export
#'
#' @examples
#' create_publication_record(data, "23056907500")
#'
create_publication_record <- function(data, scopus_id) {

  #fetch author's name based on author ID
  author_name <- do.call(rbind, data[["author"]])
  author_name <- author_name[author_name[["authid"]] == scopus_id, ]
  author_name <- author_name[["authname"]][1]

  #check if author is corresponding
  data[["corresponding"]] <- ifelse(data[["dc:creator"]] == author_name, 1, 0)

  #create table of results
  data <- data.frame(Year = as.Date(data[["prism:coverDate"]]),
                     Citations = as.numeric(data[["citedby-count"]]),
                     Corresponding = as.numeric(data[["corresponding"]]),
                     Author = author_name)

  #convert date format to year values
  data[["Year"]] <- as.numeric(substr(data[["Year"]], 1, 4))


  return(data)
}
