#' Plot publication and citation count as a marginal histogram scatter plot
#'
#' @param researcher Data frame with all relevant data for plotting
#'
#' @return marginal histogram scatter plot
#' @export
#'
#' @examples
#'
plot_publication_profile <- function(researcher) {
  
  plot <- ggplot2::ggplot(
    data = researcher,
    ggplot2::aes(x = Year,
                 y = Citations,
                 color = first_or_last)
  ) +
    ggiraph::geom_jitter_interactive(
      ggplot2::aes(
        tooltip = 
          paste(
            paste0("Title: ", Title, shiny::tags$br()),
            paste0("Published in: ", Venue)
          )
      ),
      width = 0,
      height = 0.2,
      na.rm = TRUE,
      size = 5 * sqrt(1 / researcher[["author_count"]])
    ) +
    ggplot2::geom_hline(
      yintercept = median(researcher[["Citations"]],
                          na.rm = TRUE),
      linetype = 2
    ) +
    ggplot2::geom_hline(
      yintercept = 0,
      size = 1,
      colour = "#333333"
    ) +
    ggplot2::scale_y_continuous(
      trans = "log",
      breaks = scales::log_breaks()
    ) +
    ggplot2::scale_x_continuous(
      breaks = seq(min(researcher[["Year"]]),
                   max(researcher[["Year"]]),
                   by = ifelse(
                     (max(researcher[["Year"]]) -
                        min(researcher[["Year"]])) > 20, 
                     2, 
                     1)
      )
    ) +
    ggplot2::scale_colour_manual(
      values = c("#999999", "#00509e")
    ) +
    ggplot2::scale_shape_manual(
      values = c(16, 3)
    ) +
    ggplot2::labs(
      title = " ",
      y = "Citations (log)"
    ) +
    ggplot2::theme(
      axis.text = ggplot2::element_text(
        size = 12,
        color = "#222222"
      ),
      axis.ticks.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(
        margin = ggplot2::margin(5, b = 10)
      ),
      axis.title = ggplot2::element_text(
        size = 12
      ),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(
        color="#cbcbcb"
      ),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      legend.position = "none"
    )
  
  xdens <- cowplot::axis_canvas(
    plot,
    axis = "x",
    data = researcher,
    ggplot2::aes(x = Year)
  ) +
    ggplot2::geom_histogram(
      color = "black",
      fill = "white",
      size = 0.2,
      binwidth = 1,
      na.rm = TRUE
    ) +
    ggplot2::geom_hline(
      yintercept = 0, 
      size = 1, 
      color = "#333333"
    ) +
    ggplot2::stat_bin(
      ggplot2::aes(
        y = ..count..,
        label = ifelse(..count.. > 0,
                       ..count..,
                       "")
      ),
      binwidth = 1,
      vjust = -0.5,
      pad = FALSE,
      geom = "text",
      na.rm = TRUE
    )
  
  cowplot::ggdraw(
    cowplot::insert_xaxis_grob(
      plot,
      xdens,
      grid::unit(0.2, "null"),
      position = "top"
    )
  )
  
}
