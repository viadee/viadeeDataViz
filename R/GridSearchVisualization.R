library(cowplot)
library(h2o)


#' A function that realizes and visualizes a GridSearch for Gradient Boosting Machines with H2O.
#'
#' This function allows you to further analyze GridSearches for GBM.
#' Therefore, it initializes a default modell and compares it to the parameter values in a GridSearch.
#'
#' @param train The training frame as h2O Frame
#' @param test The validation frame as h2O frame
#' @param predictorColumns A vector of columns that are used for the prediction
#' @param y The goal variable
#' @param type Distinction between "Classification" and "Regression"
#' @param runtime Max time for model training
#' @export
#' @examples
#' GridSearchGBM(train, test, predictorColumns=c("column1", "column2", "column3"), y="columnY", 360, "Classification")
GridSearchGBM= function(train, test, predictorColumns, y, runtime, type){
  if(type=="Classification"){
    metric="auc"
  }else{
    metric="rmse"
  }

  ## 1. Initialles Modell um einen Vergleichspunkt für die GridSearch zu erzeugen
  gbm1 <- h2o.gbm(
    training_frame = train,
    validation_frame = test,
    x=predictorColumns,
    y = y,
    balance_classes = FALSE, seed = 1234
  )
  summary(gbm1)

  if(metric=="rmse"){
    PerformanceInitial= h2o.rmse(gbm1, valid=TRUE)
  }else{
    PerformanceInitial= h2o.auc(gbm1, valid=TRUE)
  }


  ##2. Max_depth als Parameter per GridSearch bestimmen
  hyper_params = list( max_depth = seq(1,29,2) )

  grid_maxDepth <- h2o.grid(
    ## hyper parameters
    hyper_params = hyper_params,
    search_criteria = list(strategy = "Cartesian"),
    algorithm="gbm",
    grid_id="depth_grid",
    x = predictorColumns,
    y = y,
    training_frame = train,
    validation_frame = test,
    ntrees = 10000,
    learn_rate = 0.05,
    learn_rate_annealing = 0.99,
    sample_rate = 0.8,
    col_sample_rate = 0.8,
    seed = 1234,
    stopping_rounds = 5,
    stopping_tolerance = 1e-4,
    stopping_metric = toupper(metric),
    score_tree_interval = 10
  )

  sortedGrid <- h2o.getGrid("depth_grid", sort_by=metric, decreasing = TRUE)
  GridResultMaxDepth=as.data.frame( sortedGrid@summary_table)


  ## Plotten der erstellten Modelle mit dem Metric-Wert des initiallen Modells als Vergleichswert
  if(metric=="auc"){
    plot(GridResultMaxDepth$max_depth,GridResultMaxDepth$auc)
  }else{
    plot(GridResultMaxDepth$max_depth,GridResultMaxDepth$rmse)}
  abline(h=PerformanceInitial, col="red")

  ## Bestimmt die Grenzen in denen erneut nach der optimalen Tiefe gesucht wird und stellt diese als grüne Punkte im Graphen dar
  topDepths = sortedGrid@summary_table$max_depth[1:5]
  minDepth = min(as.numeric(topDepths))
  maxDepth = max(as.numeric(topDepths))
  if(metric=="auc"){
    points(maxDepth, GridResultMaxDepth[GridResultMaxDepth$max_depth==maxDepth,]$auc, col="green")
    points(minDepth, GridResultMaxDepth[GridResultMaxDepth$max_depth==minDepth,]$auc, col="green")
  }else{
    points(maxDepth, GridResultMaxDepth[GridResultMaxDepth$max_depth==maxDepth,]$rmse, col="green")
    points(minDepth, GridResultMaxDepth[GridResultMaxDepth$max_depth==minDepth,]$rmse, col="green")
  }

  ## 3. Durchführen einer kompletten GridSearch

  hyper_params = list(
    max_depth = seq(minDepth,maxDepth,1),
    sample_rate = seq(0.2,1,0.01),
    col_sample_rate = seq(0.2,1,0.01),
    col_sample_rate_per_tree = seq(0.2,1,0.01),
    col_sample_rate_change_per_level = seq(0.9,1.1,0.01),
    min_rows = 2^seq(0,log2(nrow(train))-1,1),
    nbins = 2^seq(4,10,1),
    nbins_cats = 2^seq(4,12,1),
    min_split_improvement = c(0,1e-8,1e-6,1e-4),
    histogram_type = c("UniformAdaptive","QuantilesGlobal","RoundRobin")
  )

  search_criteria = list(
    ## Random grid search
    strategy = "RandomDiscrete",
    max_runtime_secs = runtime,
    max_models = 200,
    seed = 1234,
    stopping_rounds = 5,
    stopping_metric = toupper(metric),
    stopping_tolerance = 1e-3
  )

  grid <- h2o.grid(
    hyper_params = hyper_params,
    search_criteria = search_criteria,
    algorithm = "gbm",
    grid_id = "final_grid",
    x = predictorColumns,
    y = y,
    training_frame = train,
    validation_frame = test,
    ntrees = 10000,
    learn_rate = 0.01,
    learn_rate_annealing = 1,
    max_runtime_secs = runtime,
    stopping_rounds = 5, stopping_tolerance = 1e-4, stopping_metric = toupper(metric),
    score_tree_interval = 10
  )



  ## Sort the grid models by rmse
  sortedGrid <- h2o.getGrid("final_grid", sort_by = metric, decreasing = TRUE)


  GridResultFinal=as.data.frame(sortedGrid@summary_table)
  GridPlotParams(GridResultFinal, PerformanceInitial, metric)
  return(sortedGrid)


}

#' Visualizes the GridSearch and saves an overview-plot as "/GridSearch.png"
#'
#' The rmse is ploted depending on the chosen parameter values in the GridSearch.
#' The best points are highlighted in blue; the ten best points of each parameter in green
#' The red line shows the comparison value of the default model.
#'
#'
#' @param GridResultFinal Descending sorted result frame
#' @param Vergleichswert The value which is used for comparison, e.g. default model
#' @param metric Used metric can be choosen between auc (Classification) and rmse (Regression)
#' @keywords
#' @examples GridPlotParams(GridResultFinal= as.data.frame(sortedGrid@summary_table), Vergleichswert= 0.8), "auc")
#'
GridPlotParams= function(GridResultFinal, Vergleichswert, metric){
  if(metric=="auc"){
    GridResultFinal$metric= as.numeric(GridResultFinal$auc)
  }else{
    GridResultFinal$metric= as.numeric(GridResultFinal$rmse)
  }
  GridResultFinal$max_depth= as.numeric(GridResultFinal$max_depth)
  GridResultFinal$min_rows= as.numeric(GridResultFinal$min_rows)
  GridResultFinal$nbins= as.numeric(GridResultFinal$nbins)
  GridResultFinal$col_sample_rate= as.numeric(GridResultFinal$col_sample_rate)
  GridResultFinal$nbins_cats= as.numeric(GridResultFinal$nbins_cats)
  GridResultFinal$sample_rate= as.numeric(GridResultFinal$sample_rate)
  GridResultFinal$col_sample_rate_per_tree= as.numeric(GridResultFinal$col_sample_rate_per_tree)
  GridResultFinal$col_sample_rate_change_per_level= as.numeric(GridResultFinal$col_sample_rate_change_per_level)
  GridResultFinal$min_split_improvement= as.numeric(GridResultFinal$min_split_improvement)

  theme_set(theme_cowplot(font_size=12))

  plot.max_depth <-ggplot(data=GridResultFinal)+
    geom_point(aes(x=GridResultFinal$max_depth, y=GridResultFinal$metric))+
    geom_point(data=GridResultFinal[1:10,], aes(x=max_depth, y=metric), colour="green")+
    geom_point(data=GridResultFinal[1,], aes(x=max_depth, y=metric), colour="blue")+
    geom_hline(yintercept = Vergleichswert, colour="red")+
    labs(
      x="Max_depths", y = metric

    )

  plot.sample_rate <-ggplot(data=GridResultFinal)+
    geom_point(aes(x=GridResultFinal$sample_rate, y=GridResultFinal$metric))+
    geom_point(data=GridResultFinal[1:10,], aes(x=sample_rate, y=metric), colour="green")+
    geom_point(data=GridResultFinal[1,], aes(x=sample_rate, y=metric), colour="blue")+
    geom_hline(yintercept = Vergleichswert, colour="red")+
    labs(
      x="sample_rate", y = metric
    )

  plot.col_sample_rate_per_tree <-ggplot(data=GridResultFinal)+
    geom_point(aes(x=GridResultFinal$col_sample_rate_per_tree, y=GridResultFinal$metric))+
    geom_point(data=GridResultFinal[1:10,], aes(x=col_sample_rate_per_tree, y=metric), colour="green")+
    geom_point(data=GridResultFinal[1,], aes(x=col_sample_rate_per_tree, y=metric), colour="blue")+
    geom_hline(yintercept = Vergleichswert, colour="red")+
    labs(
      x="col_sample_rate_per_tree", y = metric
    )

  plot.min_split_improvement <-ggplot(data=GridResultFinal)+
    geom_point(aes(x=GridResultFinal$min_split_improvement, y=GridResultFinal$metric))+
    geom_point(data=GridResultFinal[1:10,], aes(x=min_split_improvement, y=metric), colour="green")+
    geom_point(data=GridResultFinal[1,], aes(x=min_split_improvement, y=metric), colour="blue")+
    geom_hline(yintercept = Vergleichswert, colour="red")+
    labs(
      x="min_split_improvement", y = metric
    )

  plot.col_sample_rate_change_per_level <-ggplot(data=GridResultFinal)+
    geom_point(aes(x=GridResultFinal$col_sample_rate_change_per_level, y=GridResultFinal$metric))+
    geom_point(data=GridResultFinal[1:10,], aes(x=col_sample_rate_change_per_level, y=metric), colour="green")+
    geom_point(data=GridResultFinal[1,], aes(x=col_sample_rate_change_per_level, y=metric), colour="blue")+
    geom_hline(yintercept = Vergleichswert, colour="red")+
    labs(
      x="col_sample_rate_change_per_level", y = metric
    )

  plot.min_rows <-ggplot(data=GridResultFinal)+
    geom_point(aes(x=GridResultFinal$min_rows, y=GridResultFinal$metric))+
    geom_point(data=GridResultFinal[1:10,], aes(x=min_rows, y=metric), colour="green")+
    geom_point(data=GridResultFinal[1,], aes(x=min_rows, y=metric), colour="blue")+
    geom_hline(yintercept = Vergleichswert, colour="red")+
    labs(
      x="Min_rows", y = metric
    )
  plot.histogram_type <-ggplot(data=GridResultFinal)+
    geom_point(aes(x=GridResultFinal$histogram_type, y=GridResultFinal$metric))+
    geom_point(data=GridResultFinal[1:10,], aes(x=histogram_type, y=metric), colour="green")+
    geom_point(data=GridResultFinal[1,], aes(x=histogram_type, y=metric), colour="blue")+
    geom_hline(yintercept = Vergleichswert, colour="red")+
    labs(
      x="histogram_type", y = metric
    )

  plot.col_sample_rate <-ggplot(data=GridResultFinal)+
    geom_point(aes(x=GridResultFinal$col_sample_rate, y=GridResultFinal$metric))+
    geom_point(data=GridResultFinal[1:10,], aes(x=col_sample_rate, y=metric), colour="green")+
    geom_point(data=GridResultFinal[1,], aes(x=col_sample_rate, y=metric), colour="blue")+
    geom_hline(yintercept = Vergleichswert, colour="red")+
    labs(
      x="col_sample_rate", y = metric
    )

  plot.nbins_cats <-ggplot(data=GridResultFinal)+
    geom_point(aes(x=GridResultFinal$nbins_cats, y=GridResultFinal$metric))+
    geom_point(data=GridResultFinal[1:10,], aes(x=nbins_cats, y=metric), colour="green")+
    geom_point(data=GridResultFinal[1,], aes(x=nbins_cats, y=metric), colour="blue")+
    geom_hline(yintercept = Vergleichswert, colour="red")+
    labs(
      x="nbins_cats", y = metric
    )

  plot.nbins <-ggplot(data=GridResultFinal)+
    geom_point(aes(x=GridResultFinal$nbins, y=GridResultFinal$metric))+
    geom_point(data=GridResultFinal[1:10,], aes(x=nbins, y=metric), colour="green")+
    geom_point(data=GridResultFinal[1,], aes(x=nbins, y=metric), colour="blue")+
    geom_hline(yintercept = Vergleichswert, colour="red")+
    labs(
      x="nbin", y = metric
    )

  # Erzeugt einen Plot, indem alle betrachteten Parameter dargestellt sind.
  # Die erzeugten Modelle und der entsprechende rmse sind für die individuellen Parameterkombinationen als Punkte dargestellt
  # Der beste Parameterwert für jeden Parameter ist in blau dargestellt; die besten 10 in grün
  plotGrid= plot_grid(plot.max_depth,plot.min_split_improvement, plot.sample_rate,plot.col_sample_rate_per_tree, plot.min_rows, plot.nbins, plot.histogram_type, plot.nbins_cats, plot.col_sample_rate,plot.col_sample_rate_change_per_level, labels = "AUTO", hjust = 0, vjust = 1)
  ggsave(paste0(getwd(), "/GridSearch.png"), plotGrid, width = 16, height = 9, dpi = 300)
}
