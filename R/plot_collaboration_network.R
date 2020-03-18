#' Plot collaboration network
#'
#' @param network List of nodes and edges of the network
#'
#' @return network graph of publications
#' @export
#'
#' @examples
#' plot_publication_network(network)
plot_collaboration_network <- function(network){
  visNetwork::visNetwork(
    edges = network[[1]], 
    nodes = network[[2]], 
    width = "100%",
    height = "800px",
    main = list(
      text = paste("Collaboration network for ", network[[3]]),
      style = 'font-family:Open Sans,Arial,serif;font-size:22;text-align:center'
    )
  ) %>%
    visNetwork::visNodes(
      font = list(
        color = "#333333",
        size = 16,
        face = "Open Sans"
      )
    ) %>%
    visNetwork::visGroups(
      groupname = "main",
      color = "#00509e",
      shape = "square"
    ) %>%
    visNetwork::visGroups(
      groupname = "co_pub",
      color = "skyblue"
    ) %>%
    visNetwork::visOptions(
      highlightNearest = list(
        enabled = TRUE, 
        degree = 2, 
        hover = TRUE
      )
    ) %>%
    visNetwork::visPhysics(
      solver = "forceAtlas2Based"
    )
}
