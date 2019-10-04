#' Plot publication and citation count as a marginal histogram scatter plot
#'
#' @param publication_data
#'
#' @return marginal histogram scatter plot
#' @export
#'
#' @examples
#'
plot_publication_profile <- function(publication_data,
                                     author_threshold) {
  
  publication_data[["many_authors"]] <- ifelse(publication_data[["author_count"]] > author_threshold,
                                               1,
                                               0)


  # create the plot
  plot <- ggplot2::ggplot(data = publication_data,
                          ggplot2::aes(x = Year,
                              y = Citations,
                              color = factor(Corresponding),
                              shape = factor(many_authors))) +

    #specify the type of plot
    ggplot2::geom_jitter(width = 0,
                         height = 0.2,
                         na.rm = TRUE,
                         size = 2.5) +

    #add line for citation median
    ggplot2::geom_hline(yintercept = median(publication_data$Citations, na.rm = TRUE),
                        linetype = 2) +

    ggplot2::geom_hline(yintercept = 0,
                        size = 1,
                        colour="#333333") +

    #make y axis log10
    ggplot2::scale_y_continuous(trans = "log",
                                breaks = scales::log_breaks()) +

    #specify that each year shows up on x axis
    ggplot2::scale_x_continuous(breaks = seq(min(publication_data$Year),
                                             max(publication_data$Year),
                                             by = ifelse((max(publication_data$Year) -
                                                            min(publication_data$Year)) > 20,
                                                         2,
                                                         1))) +

    ggplot2::scale_colour_manual(values = c("#333333", "skyblue")) +

    #define title, subtitle and axis titles
    ggplot2::labs(title = paste0("Publication profile for ",
                                 publication_data[["Author"]][1]),
                  subtitle = "Dots: citations per publication, corresponding author coloured
                  \nHistogram: publications per year
                  \nDotted line: median number of citations per publication",
                  y = "\nCitations (log)",
                  x = "\nYear") +

    #tweak the look
    ggplot2::theme(plot.title = ggplot2::element_text(size=18,
                                                      face="bold",
                                                      color="#222222",
                                                      hjust = 0),
                   plot.subtitle = ggplot2::element_text(size = 12,
                                                         margin = ggplot2::margin(5, 0, 15, 0)),
                   axis.text = ggplot2::element_text(size=10,
                                                     color="#222222"),
                   axis.text.x = ggplot2::element_text(margin = ggplot2::margin(5, b = 10)),

                   panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major.y = ggplot2::element_line(color="#cbcbcb"),
                   panel.grid.major.x = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   legend.position = "none"
    )

  # add histogram of number of publications on the top of the graph
  xdens <- cowplot::axis_canvas(plot,
                                axis = "x",
                                data = publication_data,
                                ggplot2::aes(x = Year)) +
    ggplot2::geom_histogram(color = "black",
                            fill = "white",
                            size = 0.2,
                            binwidth = 1,
                            na.rm = TRUE) +
    ggplot2::stat_bin(ggplot2::aes(y = ..count..,
                          label = ifelse(..count.. > 0,
                                         ..count..,
                                         "")),
                      binwidth = 1,
                      vjust = -0.5,
                      pad = FALSE,
                      geom = "text",
                      na.rm = TRUE)

  cowplot::ggdraw(cowplot::insert_xaxis_grob(plot,
                                             xdens,
                                             grid::unit(0.2, "null"),
                                             position = "top"))
}
