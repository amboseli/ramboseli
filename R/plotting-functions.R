greys_palette <- c("#FFFFFF", "#F0F0F0", "#D9D9D9", "#BDBDBD", "#969696",
                   "#737373", "#525252", "#252525", "#000000")

#' A ggplot2 theme with internal tick marks.
#'
#' @return A ggplot2 theme.
#' @export
#'
theme_internal_ticks <- function() {

  palette <- greys_palette

  color.background = "black"
  color.grid.major = "black"
  color.axis.text = "black"
  color.axis.title = "black"
  color.title = "black"

  # Begin construction of chart
  theme_bw(base_size = 9) +

    # Set the entire chart region to a light gray color
    theme(panel.background = element_rect(fill = "white", color = color.background),
          panel.border = element_rect(color = "black")) +

    # Format the grid
    theme(panel.grid = element_blank()) +

    # Format ticks
    theme(axis.ticks = element_line(colour = "black"),
          axis.ticks.length = unit(-0.125, "cm"),
          axis.text.x = element_text(margin = unit(c(0.25, 0.25, 0.25, 0.25), "cm"),
                                     size = 7),
          axis.text.y = element_text(margin = unit(c(0.25, 0.25, 0.25, 0.25), "cm"),
                                     size = 7)) +

    # Format the legend, but bottom by default
    theme(legend.position = "bottom",
          legend.key.width = unit(0.5, "cm"),
          legend.key = element_blank(),
          legend.background = element_blank(),
          legend.text = element_text(size = 7,color = color.axis.title)) +

    # Format facets
    theme(strip.background = element_blank()) +
    theme(strip.text = element_text(face = "bold")) +

    #panel.border = element_rect(colour = "black"),


    # Set title and axis labels, and format these and tick marks
    theme(plot.title = element_text(color = color.title, size = 10,
                                    vjust = 1.25, hjust = 0.5),
          axis.text.x = element_text(size = 7,color = color.axis.text),
          axis.text.y = element_text(size = 7,color = color.axis.text),
          axis.title.x = element_text(size = 8,color = color.axis.title, vjust = 0),
          axis.title.y = element_text(size = 8,color = color.axis.title, vjust = 1.25)) +

    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.35, 0.35, 0.35), "cm"))
}


#' A doubled version of theme_internal_ticks.
#'
#' @return A ggplot2 theme.
#' @export
#'
theme_internal_ticks_x2 <- function() {

  palette <- greys_palette
  color.background = "black"
  color.grid.major = "black"
  color.axis.text = "black"
  color.axis.title = "black"
  color.title = "black"

  # Begin construction of chart
  theme_bw(base_size = 18) +

    # Set the entire chart region to a light gray color
    theme(panel.background = element_rect(fill = "white", color = color.background),
          panel.border = element_rect(color = "black", size = 1)) +

    # Format the grid
    theme(panel.grid = element_blank()) +

    # Format ticks
    theme(axis.ticks = element_line(colour = "black"),
          axis.ticks.length = unit(-0.125, "cm"),
          axis.text.x = element_text(margin = unit(c(0.25, 0.25, 0.25, 0.25), "cm"),
                                     size = 14),
          axis.text.y = element_text(margin = unit(c(0.25, 0.25, 0.25, 0.25), "cm"),
                                     size = 14)) +

    # Format the legend, but bottom by default
    theme(legend.position = "bottom",
          legend.key.width = unit(0.5, "cm"),
          legend.key = element_blank(),
          legend.background = element_blank(),
          legend.text = element_text(size = 14, color = color.axis.title)) +

    # Format facets
    theme(strip.background = element_blank()) +
    theme(strip.text = element_text(face = "bold")) +

    #panel.border = element_rect(colour = "black"),


    # Set title and axis labels, and format these and tick marks
    theme(plot.title = element_text(color = color.title, size = 20,
                                    vjust = 1.25, hjust = 0.5),
          axis.text.x = element_text(size = 14, color = color.axis.text),
          axis.text.y = element_text(size = 14, color = color.axis.text),
          axis.title.x = element_text(size = 16, color = color.axis.title, vjust = 0),
          axis.title.y = element_text(size = 16, color = color.axis.title, vjust = 1.25)) +

    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.35, 0.35, 0.35), "cm"))
}


#' A clean ggplot2 theme with no internal grid lines.
#'
#' @return A ggplot2 theme.
#' @export
#'
theme_journal <- function() {

  palette <- greys_palette
  color.background = "black"
  color.grid.major = "black"
  color.axis.text = "black"
  color.axis.title = "black"
  color.title = "black"

  # Begin construction of chart
  theme_bw(base_size = 9) +

    # Set the entire chart region to a light gray color
    theme(panel.background = element_rect(fill = "white", color = color.background),
          panel.border = element_rect(color = "black")) +

    # Format the grid
    theme(panel.grid = element_blank()) +

    # Format the legend, but bottom by default
    theme(legend.position = "bottom",
          legend.key.width = unit(0.5, "cm"),
          legend.key = element_blank(),
          legend.background = element_blank(),
          legend.text = element_text(size = 7,color = color.axis.title)) +

    # Format facets
    theme(strip.background = element_blank()) +
    theme(strip.text = element_text(face = "bold")) +

    #panel.border = element_rect(colour = "black"),


    # Set title and axis labels, and format these and tick marks
    theme(plot.title = element_text(color = color.title, size = 10,
                                    vjust = 1.25, hjust = 0.5),
          axis.text.x = element_text(size = 7,color = color.axis.text),
          axis.text.y = element_text(size = 7,color = color.axis.text),
          axis.title.x = element_text(size = 8,color = color.axis.title, vjust = 0),
          axis.title.y = element_text(size = 8,color = color.axis.title, vjust = 1.25)) +

    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.35, 0.35, 0.35), "cm")) +

    # Title aligned left
    theme(plot.title = element_text(hjust = 0))
}


#' A doubled version of theme_journal.
#'
#' @return A ggplot2 theme.
#' @export
#'
theme_journal_x2 <- function() {

  palette <- greys_palette
  color.background = "black"
  color.grid.major = "black"
  color.axis.text = "black"
  color.axis.title = "black"
  color.title = "black"

  # Begin construction of chart
  theme_bw(base_size = 18) +

    # Set the entire chart region to a light gray color
    theme(panel.background = element_rect(fill = "white", color = color.background),
          panel.border = element_rect(color = "black", size = 1)) +

    # Format the grid
    theme(panel.grid = element_blank()) +

    # Format the legend, but bottom by default
    theme(legend.position = "bottom",
          legend.key.width = unit(0.5, "cm"),
          legend.key = element_blank(),
          legend.background = element_blank(),
          legend.text = element_text(size = 14, color = color.axis.title)) +

    # Format facets
    theme(strip.background = element_blank()) +
    theme(strip.text = element_text(face = "bold")) +

    #panel.border = element_rect(colour = "black"),


    # Set title and axis labels, and format these and tick marks
    theme(plot.title = element_text(color = color.title, size = 20,
                                    vjust = 1.25, hjust = 0.5),
          axis.text.x = element_text(size = 14, color = color.axis.text),
          axis.text.y = element_text(size = 14, color = color.axis.text),
          axis.title.x = element_text(size = 16, color = color.axis.title, vjust = 0),
          axis.title.y = element_text(size = 16, color = color.axis.title, vjust = 1.25)) +

    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.35, 0.35, 0.35), "cm")) +

    # Title aligned left
    theme(plot.title = element_text(hjust = 0))
}

#' A signed square-root transform.
#' This transform can be useful when you want to square-root transform
#' a plot axis to emphasize variation in the small values, but the data
#' contain negative values.
#' @return A transformation
#' @export
#'
sqrt_sign_trans <-  function(){
  scales::trans_new('sqrt_sign', transform = function(x) sign(x) * sqrt(abs(x)),
            inverse = function(x) sign(x) * x^2)
}
