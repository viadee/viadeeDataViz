library(ggplot2)
library(grid)
library(ggthemes)


#' A function that styles a ggplot
#'
#' This function allows you give your plots a viadee background
#' and some styling based on the economist theme.
#' Ideally this is the first thing you do with your graph
#' object, in order not to overpaint any content.
#'
#' @param graph The graph to style.
#' @param backgroundGradientColors The two colors that form a gradient in the background
#' @keywords Background
#' @export
#' @examples
#' viadeePlot(ggplot2::ggplot()) + ggplot2::geom_point()

viadeePlot<-function(graph, backgroundGradientColors = c("#DFAD47","#7EBCA9")) # Default: Yellow-to-cyan
{
  vc=t(backgroundGradientColors)
  vg <- rasterGrob(vc, width=unit(1,"npc"), height = unit(1,"npc"),
                   interpolate = TRUE) # Colored Background
  sl <- grid.polygon(x=c(0.00, 0.90, 0.91, 0.94, 0.0),
                     y=c(0.98, 0.98, 0.96, 1.00, 1.0), draw=FALSE,
                     gp=gpar(col="#D5E4EB", fill="#D5E4EB")) # Relative coordinates i.e. "percentages" where (0,0) is the bottom left

  return( graph
          + theme_economist()
          + scale_color_brewer(palette="Spectral")
          + scale_fill_brewer(palette="Spectral")
          + annotation_custom(vg, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
          + annotation_custom(sl, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
          + theme(
            axis.text.x = element_text(size=7),
            axis.text.y = element_text(size=7),
            axis.title.x = element_text(size=10),
            axis.title.y = element_text(size=10),
            legend.text = element_text(size=5))
  )
}


#' A function that styles a ggplot
#'
#' This function allows you give your plots a viadee background
#' and some styling based on the economist theme.
#' Ideally this is the first thing you do with your graph
#' object, in order not to overpaint any content.
#'
#' @param graph The graph to style with a red-to-yellow background.
#' @keywords Background
#' @export
#' @examples
#' viadeePlot_fire(ggplot2::ggplot()) + ggplot2::geom_point()

viadeePlot_fire<-function(graph)
{
  return (viadeePlot(graph, c("#E5344E","#DFAD47"))) # red-to-yelloW
}

#' A function that styles a ggplot
#'
#' This function allows you give your plots a viadee background
#' and some styling based on the economist theme.
#' Ideally this is the first thing you do with your graph
#' object, in order not to overpaint any content.
#'
#' @param graph The graph to style.
#' @keywords Background
#' @export
#' @examples
#' viadeePlot_fire(ggplot2::ggplot()) + ggplot2::geom_point()

viadeePlot_purple<-function(graph)
{
  return (viadeePlot(graph, c("#7EBCA9","#E5344E")))
}
