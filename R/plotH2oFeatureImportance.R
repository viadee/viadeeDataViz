library(ggplot2)
library(h2o)

#' A function that creates a styled h2o variable importance plot
#'
#' @param mod The model to query for feature importances
#' @param threshold Features with less relative importance than the threshold will be omitted (defaults to 0.005)
#' @keywords h2o
#' @export
h2oViz.plotVariableImportance <- function(mod, threshold = 0.005)
{
  varimp <- h2o.varimp(mod) %>% filter(percentage > threshold)

  g <- viadeePlot(ggplot(varimp)) +
    geom_bar(aes(x=reorder(variable,percentage),
                 weight=percentage),
             fill="white",
             alpha = 0.8) +
    geom_text(aes(label=round(percentage,3), x=reorder(variable,percentage),
                  y=percentage), color="black", size=2.5)+
    coord_flip() +
    labs(title = "Faktorgewichtung",
         subtitle = mod@model_id,
         x = "Faktoren", y = "Faktorgewichtung (relativ)")
  return(g)
}
