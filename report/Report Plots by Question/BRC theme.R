theme_brc <- function(...) {
  # font <- "Arial"
  font <- NULL
  
  ggplot2::theme(
    # Plot title
    plot.title.position = "plot",
    plot.title = ggplot2::element_text(family = font,
                                       size = 16,
                                       face = "bold",
                                       color = brc_colours$black_shadow),
    
    plot.subtitle = ggplot2::element_text(family = font,
                                          size = 12,
                                          color = brc_colours$black_shadow),
    
    plot.caption.position = "plot",
    plot.caption = ggplot2::element_text(family = font,
                                         size = ggplot2::rel(0.8),
                                         color = brc_colours$black_shadow,
                                         hjust = 0),
    
    axis.text = ggplot2::element_text(family = font, size = ggplot2::rel(0.8), colour = brc_colours$black_shadow),
    axis.title = ggplot2::element_text(family = font, face = "bold", colour = brc_colours$black_shadow),
    
    # no background
    panel.border = ggplot2::element_blank(),
    panel.background = ggplot2::element_rect(fill = "white", colour = NA),
    
    # make gridlines dark, same contrast with white as in theme_grey
    panel.grid = ggplot2::element_line(colour = "grey92"),
    panel.grid.major = ggplot2::element_line(size = ggplot2::rel(0.8)),
    panel.grid.minor = ggplot2::element_line(size = ggplot2::rel(0.4)),
    
    # show axes
    axis.line = ggplot2::element_line(colour = brc_colours$black_shadow, size = ggplot2::rel(1)),
    
    # match legend key to panel.background
    legend.key = ggplot2::element_blank(),
    
    # simple, black and white strips for facets
    strip.background = ggplot2::element_rect(fill = "white", colour = brc_colours$black_shadow, size = ggplot2::rel(2)),
    strip.text = ggplot2::element_text(family = font, hjust = 0),
    
    ...
  )
}