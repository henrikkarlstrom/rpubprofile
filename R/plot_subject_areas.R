#' Plot the most active research subjects as an ordered bar chart
#'
#' @param field_data
#'
#' @return Bar chart
#' @export
#'
#' @examples
#'
#'
plot_field_data <- function(field_data) {

  #selects the top five fields
  ggplot2::ggplot(data = field_data[1:5, ],
                  aes(x = Var1,
                      y = Freq)) +

    #defines bar chart and fill colour
    ggplot2::geom_bar(stat = "identity",
                      fill = "#007f7f") +

    #flips the coordinates
    ggplot2::coord_flip() +
    ggplot2::geom_hline(yintercept = 0,
                        size = 1,
                        colour="#333333") +

    #sets the plot title and removes axis titles
    ggplot2::labs(title = "Most active research subjects",
                  x = ggplot2::element_blank(),
                  y = "Number of publications") +

    #specifies a lot of minor aesthetic details
    ggplot2::theme(plot.title = ggplot2::element_text(size=18,
                                                      face="bold",
                                                      color="#222222",
                                                      hjust = 0,
                                                      margin = ggplot2::margin(5, 0, 15, 0)),
                   axis.text = ggplot2::element_text(size=10,
                                                     color="#222222"),
                   axis.text.x = ggplot2::element_text(margin = margin(5, b = 10)),
                   axis.ticks = ggplot2::element_blank(),
                   axis.line = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major.y = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_line(color="#cbcbcb"),
                   panel.background = ggplot2::element_blank(),
                   legend.position = "none"
    )
}
