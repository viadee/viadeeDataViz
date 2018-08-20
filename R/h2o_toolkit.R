library(h2o)

#' Return the best model from a h2o grid search
#'
#' The function passes further parameters, such as the search criteria, directly
#' to the h2o.grid() function. These parameters have to be explicitly named.
#' A new h2o instance is initialised. Pass startH2O=TRUE if that is not wanted.
#'
#' @param data The dataframe that is used for training and validating the
#'   models.
#' @param target The name of the target variable.
#' @param algorithm A string that contains the name of the algorithm for the
#'   models (gbm, randomForest, kmeans, glm, deeplearning, naivebayes, pca)
#' @param hyperparameters this is either a list of sequences for the values that
#'   the grid search shall cover, or a list of concrete parameters
#' @param loading boolean value that is True when the function is used to
#'   instantiate a new model with specific parameters
#' @export
#' @examples
#' get_model(data, "duration", "gbm", hyper_parameter_list, search_criteria=search_criteria_list)
get_model <-
  function(data,
           target,
           algorithm,
           hyperparameters,
           loading = FALSE,
           ... ){

    if (loading==FALSE & missing(algorithm)) {
      stop("Algorithm name must be given.", call. = FALSE)
    }

    if(loading==FALSE & missing(target)){
      stop("Target variable must be given.", call. = FALSE)
    }

    h2o.init(max_mem_size="8G")
    h2o.removeAll()

    data_h2O= as.h2o(data)
    complete_h2o_split <- h2o.splitFrame(data=data_h2O, ratios=0.75)
    Train <- complete_h2o_split[[1]]
    Test <- complete_h2o_split[[2]]


    if(loading){
      #in this case all needed variables are taken from the given hyperparameters
      gbm_grid <-do.call(h2o.grid, c(training_frame = Train,
                                     validation_frame = Test,
                                     hyperparameters))
    }else{
      gbm_grid <- h2o.grid(
        algorithm = algorithm,
        x = names(Train),
        y = target,
        training_frame = Train,
        validation_frame = Test,
        hyper_params = hyperparameters,
        ...
      )
    }

    #return only the best model
    return(h2o.getModel(gbm_grid@model_ids[[1]]))


  }





#  saving and loading -----------------------------------------------------

#' Saves a h2o model object
#'
#' Serializes a h2o model object's parameters to the specified file path for
#' later reinstantiation. Currently the path needs to be a concrete file path
#' with the .rdata ending.
#'
#'
#' @param model A h2o model object that shall be saved
#' @param path A string with a correct file path
#' @keywords
#' @examples save_model(gbm_model, "./trained_models/model_parameters_Test.rdata")
#'
save_model <- function(model, path){
  parameters <- model@allparameters
  parameters$algorithm <- model@algorithm
  dput(parameters, path)
}

#' Loads a h2o model object
#'
#' The parameters from the serialized model object are read and a grid search
#' ist called with these parameters. To do this, data is needed to train the
#' model. Ideally, this is the same data with which the saved model was trained.
#' The new model object is returned.
#'
#'
#' @param path A string with a correct file path
#' @param data data for instantiating the model
#' @examples load_model("./trained_models/model_parameters_Test.rdata", cases_processed)
#'
load_model <- function(path, data){
  loaded_parameters <- dget(path)

  #need to be removed since these entries contain only the names of the
  #environments and else lead to parameter conflicts in the h2o.grid() function
  loaded_parameters$training_frame <- NULL
  loaded_parameters$validation_frame <- NULL

  model <- get_model(data, hyperparameters = loaded_parameters, loading = TRUE)
  return(model)
}



#' Save information about a trained model in a csv file
#'
#' Save information about a trained model in a csv file to compare model performance
#' over the lifetime of a running project. So far this function has only been tested for
#' binomial and regression models.
#'
#'
#' @param model a h2o model
#' @param data the data that was used for training and testing the model
#' @param path the path to where the csv shall be saved (optional)
#' @param split_seed if a seed was used to split the data in training and test set, this seed
#' can also be saved (optional)
#' @param custom_names custom variables can be added to the file. This variable
#'   should contain the names of the columns as a list of strings (optional)
#' @param custom_values the values corresponding to the column names in custom_names. Lists as values are not supported! (optional)
#' @param custom_threshold the threshold that shall be used to determine the values of the different measures  (optional)
#' @export
#' @examples
#' save_score(gbm, checkedCases, custom_names=c("ersparnisProFall", "optimalThreshold"), custom_values=c(Optimal, optimalThreshold), custom_threshold = optimalThreshold)
save_score <-
  function(model,
           data,
           path = "",
           split_seed = NA,
           custom_names = NULL,
           custom_values = NULL,
           custom_threshold = NULL) {
    #browser()

    # zuerst wird versucht die bisherigen Werte einzulesen
    model_highscore_old <- tryCatch(
      {
        read.csv(paste(path,"model_highscore.csv", sep = "")) %>%
          select(-X) %>% #diese Spalte entsteht automatisch beim Einlesen
          mutate(date = as.POSIXct(date)) #nötig für späteres mergen
      },
      warning=function(cond) {
        message("Path is incorrect or there is no highscore yet saved")
        # sollten keine Daten vorhanden sein wird die Variable auf NA gesetzt, damit
        # dies später erkannt werden kann
        NA
      }
    )

    # wird gebraucht um bestimmte metrics zu bekommen
    perf <- h2o.performance(model, valid=TRUE)

    # alle spalten die unabhängig von der Kategorie gespeichert werden sollen
    date <- Sys.time()
    name <- model@model_id
    category <- perf@metrics$model_category
    scoring_history_training <- NA #Listen werden zunächst mit NA intialisiert und später hinzugefügt
    scoring_history_validation <- NA
    training_time <-
      model@model$run_time  #ich hab in der Doku nicht gefunden welche Zeit genau in dieser Variable
    #gespeichtert ist aber ich gehe davon aus, dass es die Trainingszeit ist
    seed <- model@allparameters$seed
    split_seed <- split_seed
    data_length <- nrow(data)
    features <-  NA
    important_features <- NA

    if(perf@metrics$model_category=="Regression"){

      #regressionsspezifische eigenschaften
      RMSE <- perf@metrics$RMSE
      MSE <- perf@metrics$MSE
      MAE <- perf@metrics$mae
      RMSLE <- perf@metrics$rmsle

      model_highscore_new <- data.frame(date,
                                        name,
                                        category,
                                        RMSE,
                                        MAE,
                                        MSE,
                                        RMSLE,
                                        scoring_history_training,
                                        scoring_history_validation,
                                        training_time,
                                        seed,
                                        split_seed,
                                        data_length,
                                        features,
                                        important_features,
                                        stringsAsFactors = FALSE
      )

    }else if(perf@metrics$model_category=="Binomial"){
      #klassifikationsspezifische eigenschaften
      if(missing(custom_threshold)){
        # falls keine spezieller threshold angegeben wurde, kann nur die confusion matrix
        # gesetzt werden
        confusion_matrix <- h2o.confusionMatrix(model)
        #statt null hätte ich hier lieber das default threshold das h2o benutzt
        #(um z.B. die confusion matrix anzuzeigen), aber ich weiß nicht nach
        #welcher measure das bestimmt wird
        sensitivity <- NULL
        specificity <- NULL
        accuracy <-NULL
        precision <- NULL
        fmeasure <- NULL
      }else{
        confusion_matrix <- h2o.confusionMatrix(model, thresholds = 1-custom_threshold)
        sensitivity <- h2o.sensitivity(perf, thresholds = 1-custom_threshold)[[1]]
        specificity <- h2o.specificity(perf, thresholds = 1-custom_threshold)[[1]]
        #die nächsten 3 werden bisher nicht mitgespeichert, könnten aber hinzugefügt werden
        accuracy <- h2o.accuracy(perf, thresholds = 1-custom_threshold)[[1]]
        precision <- h2o.precision(perf, thresholds = 1-custom_threshold)[[1]]
        fmeasure <- h2o.F1(perf, thresholds = 1-custom_threshold)[[1]]
      }
      AUC <- perf@metrics$AUC
      meanPerClassError <- perf@metrics$mean_per_class_error
      TrueNegatives <- confusion_matrix$"FALSE"[1]
      FalseNegatives <- confusion_matrix$"TRUE"[1]
      TruePositives <- confusion_matrix$"TRUE"[2]
      FalsePositives <- confusion_matrix$"FALSE"[2]


      model_highscore_new <- data.frame(date,
                                        name,
                                        category,
                                        AUC,
                                        meanPerClassError,
                                        TrueNegatives,
                                        FalseNegatives,
                                        TruePositives,
                                        FalsePositives,
                                        sensitivity,
                                        specificity,
                                        scoring_history_training,
                                        scoring_history_validation,
                                        training_time,
                                        seed,
                                        split_seed,
                                        data_length,
                                        features,
                                        important_features,
                                        stringsAsFactors = FALSE
      )



    }else{
      message("model category not supported")
    }

    #alle Felder die Listen enthalten werden nun hinzugefügt
    if(perf@metrics$model_category=="Regression"){

      model_highscore_new$scoring_history_training <-
        as.character(list(model@model$scoring_history$training_rmse))
      model_highscore_new$scoring_history_validation <-
        as.character(list(model@model$scoring_history$validation_rmse))

    }else if(perf@metrics$model_category=="Binomial"){

      model_highscore_new$scoring_history_training <-
        as.character(list(model@model$scoring_history$training_auc))
      model_highscore_new$scoring_history_validation <-
        as.character(list(model@model$scoring_history$validation_auc))

    }else{
      message("model category not supported")
    }
    model_highscore_new$important_features <- as.character(list(model@model$variable_importances$variable))
    model_highscore_new$features <- as.character(list(names(data)))


    #custom features werden hinzugefügt
    for(i in 1:length(custom_names)){
      #ein einspaltiges dataframe wird pro feature erstellt
      # Listen würden hier ein Problem darstellen, da dann dieses Dataframe mehr als eine Zeile hätte
      feature <- data.frame("variable"=custom_values[i])
      #und dann dem dataframe hinzugefügt
      model_highscore_new <- cbind(model_highscore_new, feature)
      #das ganze muss so umständlich erstellt werden, da man erst jetzt den Spaltennamen auf
      #einen Variablenwert setzen kann
      names(model_highscore_new)[names(model_highscore_new)=="variable"] <- custom_names[i]
    }


    #Schreiben des neuen scores in eine neue Datei oder in die alte Datei
    if(is.na(model_highscore_old)){
      write.csv(model_highscore_new, file = paste(path,"model_highscore.csv", sep = ""))
    }else{
      model_highscore <- rbind(model_highscore_old, model_highscore_new)
      write.csv(model_highscore, file = paste(path,"model_highscore.csv", sep = ""))
    }
  }


