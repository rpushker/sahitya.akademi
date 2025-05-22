#' apply_z_theme
#' @title Apply Z Theme Colors provided by Zoni Nation -> Perceptions
#' @description
#' Apply Z Theme Colors provided by Zoni Nation -> Perceptions available at
#' https://github.com/zonination/perceptions. The code is inspired by ztheme.R
#' file available within perceptions package at
#' https://github.com/zonination/perceptions/blob/master/ztheme.R.
#' @author Zoni Nation - https://github.com/zonination
#' @return A theme object.
#' @examples
#' apply_z_theme()
#' @import RColorBrewer
#' @export
apply_z_theme <- function() {
  # Generate the colors for the chart procedurally with RColorBrewer
  palette <- RColorBrewer::brewer.pal("Greys", n = 9)
  color_background <- palette[2]
  color_grid_major <- palette[3]
  color_axis_text <- palette[7]
  color_axis_title <- palette[7]
  color_title <- palette[8]

  # Begin construction of chart
  theme_bw(base_size = 9) +

    # Set the entire chart region to a light gray color
    theme(panel.background = element_rect(fill = color_background,
                                          color = color_background)) +
    theme(plot.background = element_rect(fill = color_background,
                                         color = color_background)) +
    theme(panel.border = element_rect(color = color_background)) +

    # Format the grid
    theme(panel.grid.major = element_line(color = color_grid_major,
                                          size = .25)) +
    theme(panel.grid.minor = element_blank()) +
    theme(axis.ticks = element_blank()) +

    # Format the legend, but hide by default
    theme(legend.position = "none") +
    theme(legend.background  =  element_rect(fill = color_background)) +
    theme(legend.text  =  element_text(size = 7, color = color_axis_title)) +
    # Set title and axis labels, and format these and tick marks
    theme(plot.title = element_text(color = color_title, size = 20,
                                    vjust = 1.25)) +
    theme(axis.text.x = element_text(size = 14, color = color_axis_text)) +
    theme(axis.text.y = element_text(size = 14, color = color_axis_text)) +
    theme(axis.title.x = element_text(size = 16, color = color_axis_title,
                                      vjust = 0)) +
    theme(axis.title.y = element_text(size = 16, color = color_axis_title,
                                      vjust = 1.25))
}
