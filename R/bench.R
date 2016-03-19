#' FUNCTION: benchmark
#'
#' This function runs multiple trials of experimentation
#' on a list of programs using a list of
#' different machine learning algorithms
#' @export

#programs <- list("bpmail", "netweaver","diebierse","geogoogle","hftbomberman","inspirento","jnfe","jniinchi","lagoon","lavalamp","schemaspy","xisemele")
#programs <- list("bpmail", "netweaver")

#Rmspredict::graph_cpred(df)
# df$value was a char so as.numeric turns it into a double
# found this out using the df %>% dplyr::glimpse() function to
# see the variable types

benchmark <- function(trials, methods, programs,
                      data, type, sampling = "regular", kfolds = 10, repeats = 1) {
  data.list <- list()
  for (i in 1:length(programs)) {
    data.list[[i]] <- Rmspredict::pred_bench(trials = trials, pred = programs[[i]], data = data, kfolds=kfolds,
                                             methods = methods, sampling = sampling, type = type, repeats=repeats)
  }
  df <- do.call("rbind", data.list)
  df$value <- as.numeric(df$value)
  return(df)
}

#' FUNCTION: pred_bench
#'
#' This function runs multiple trials of experimentation on a list of machine learning algorithm
#' using the same data
#' @export

#methods <- list("gbm", "svmRadial", "parRF","C5.0", "rf")
#methods <- list("svmRadial", "parRF", "majority")

pred_bench <- function(trials, pred, data, methods, sampling, type, kfolds, repeats) {
  print(pred)
  acc.output <- matrix(ncol = length(methods), nrow = trials)
  for (j in 1:length(methods)) {
    print(methods[[j]])
    for (i in 1:trials) {
      acc.output[i,j] <- Rmspredict::pred_frame(method=methods[[j]], pred=pred, data=data,
                                                sampling=sampling, type=type, kfolds=kfolds, repeats=repeats)
      print(acc.output[i,j])
    }
  }

  Program <- rep(pred, length(acc.output[[j]]))
  acc.output <- cbind(Program, acc.output)
  colnames(acc.output) <- c("Program", methods)
  acc.output <- as.data.frame(acc.output)
  acc.output <- melt(acc.output, id = "Program")

  return(acc.output)
}

#' FUNCTION: pred_frame
#'
#' This function provides a frame for all possible prediction
#' configurations

pred_frame <- function(method, pred, data, sampling, type, kfolds, repeats) {
  if (type == "classification") {
    if (method == "majority") {
      return(cat_majority(data, pred))
    }
    else if (method == "random") {
      return(cat_rand(data,pred))
    }
    else {
      return(cat_prediction(data,method,pred, kfolds, repeats))
    }
  }
  else if (type == "regression") {
    if (method == "average") {
      avg_preds <- reg_average_pred(data,pred)
      rmse <- matrix(unlist(avg_preds["rmse"]), ncol = 1, byrow = TRUE)
      mae <- matrix(unlist(avg_preds["mae"]), ncol = 1, byrow = TRUE)
      return(rmse)
    }
    else {
      num_preds <- num_predict(data,method,pred)
      rmse <- matrix(unlist(num_preds["rmse"]), ncol = 1, byrow = TRUE)
      mae <- matrix(unlist(num_preds["mae"]), ncol = 1, byrow = TRUE)
      return(rmse)
    }
  }

  else {
    stop("type needs to be either 'classification' or 'regression'")
  }
}
