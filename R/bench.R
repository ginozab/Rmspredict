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
  if (type == "classification") {
    df$Accuracy <- as.numeric(df$Accuracy)
    df$Kappa <- as.numeric(df$Kappa)
  }
  else {
    df$RMSE <- as.numeric(df$RMSE)
    df$MAE <- as.numeric(df$MAE)
  }
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
  #print(pred)
  acc.output <- matrix(ncol = length(methods), nrow = trials)
  kap.output <- matrix(ncol = length(methods), nrow = trials)
  for (j in 1:length(methods)) {
    #print(methods[[j]])
    for (i in 1:trials) {
      acc.output[i,j] <- Rmspredict::pred_frame(method=methods[[j]], pred=pred, data=data,
                                                sampling=sampling, type=type, kfolds=kfolds, repeats=repeats, eval = 1)
      kap.output[i,j] <- Rmspredict::pred_frame(method=methods[[j]], pred=pred, data=data,
                                                sampling=sampling, type=type, kfolds=kfolds, repeats=repeats, eval = 2)

      print(pred)
      print(methods[[j]])
      print("Eval One")
      print(acc.output[i,j])
      print("Eval Two")
      print(kap.output[i,j])
      print("")
    }
  }

  acc.output <- data_prep(acc.output, pred, j, methods)
  acc.output <- melt(acc.output, id = c("Trial", "Program"))


  kap.output <- data_prep(kap.output, pred, j, methods)
  kap.output <- melt(kap.output, id = c("Trial", "Program"))


  if (type == "classification") {
    colnames(acc.output)[which(names(acc.output) == "value")] <- "Accuracy"
    colnames(kap.output)[which(names(kap.output) == "value")] <- "Kappa"
  }
  else {
    colnames(acc.output)[which(names(acc.output) == "value")] <- "RMSE"
    colnames(kap.output)[which(names(kap.output) == "value")] <- "MAE"
  }

  output <- dplyr::full_join(acc.output, kap.output, by=c("Trial", "Program", "variable"))
  colnames(output)[which(names(output) == "variable")] <- "Algorithm"

  return(output)
}

#' FUNCTION: data_prep
#'
#' This function gets the data in tidy format after trials have been run

data_prep <- function(data, pred, j, methods) {
  Program <- rep(pred, length(data[[j]]))
  data <- cbind(Program, data)
  data <- cbind(order=seq(nrow(data)), data)
  colnames(data) <- c("Trial", "Program", methods)
  data <- as.data.frame(data)
  return(data)
}

#' FUNCTION: pred_frame
#'
#' This function provides a frame for all possible prediction
#' configurations

pred_frame <- function(method, pred, data, sampling, type, kfolds, repeats, eval) {
  if (type == "classification") {
    if (method == "majority") {
      retlist <- cat_majority(data, pred)

      if (eval == 1) {
        return(matrix(unlist(retlist["acc"]), ncol = 1, byrow = TRUE))
      }
      else {
        return(matrix(unlist(retlist["kap"]), ncol = 1, byrow = TRUE))
      }

    }
    #else if (method == "random") {
    #  retlist <- cat_rand(data, pred)

    #  if (eval == 1) {
    #    return(matrix(unlist(retlist["acc"]), ncol = 1, byrow = TRUE))
    #  }
    #  else {
    #    return(matrix(unlist(retlist["kap"]), ncol = 1, byrow = TRUE))
    #  }

    #}
    else {
      retlist <- cat_prediction(data,method,pred, kfolds, repeats)

      if (eval == 1) {
        return(matrix(unlist(retlist["acc"]), ncol = 1, byrow = TRUE))
      }
      else {
        return(matrix(unlist(retlist["kap"]), ncol = 1, byrow = TRUE))
      }

    }
  }


  else if (type == "regression") {
    if (method == "average") {
      avg_preds <- reg_average_pred(data,pred)
      if (eval == 1) {
        return(matrix(unlist(avg_preds["rmse"]), ncol = 1, byrow = TRUE))
      }
      else {
        return(matrix(unlist(avg_preds["mae"]), ncol = 1, byrow = TRUE))
      }
    }
    else {
      num_preds <- num_predict(data,method,pred)
      if (eval == 1) {
        return(matrix(unlist(num_preds["rmse"]), ncol = 1, byrow = TRUE))
      }
      else {
        return(matrix(unlist(num_preds["mae"]), ncol = 1, byrow = TRUE))
      }
    }
  }


  else {
    stop("type needs to be either 'classification' or 'regression'")
  }
}
