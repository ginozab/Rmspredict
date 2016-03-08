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
                      data = totalCDCatEvo, sampling = "regular", class = NULL) {
  data.list <- list()
  for (i in 1:length(programs)) {
    data.list[[i]] <- Rmspredict::pred_bench(trials = trials, pred = programs[[i]], data = data,
                                             methods = methods, sampling = sampling, class = class)
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

pred_bench <- function(trials, pred, data, methods, sampling, class) {
  print(pred)
  acc.output <- matrix(ncol = length(methods), nrow = trials)
  for (j in 1:length(methods)) {
    print(methods[[j]])
    for (i in 1:trials) {
      acc.output[i,j] <- Rmspredict::pred_frame(method=methods[[j]], pred=pred, data=data,
                                                sampling=sampling, class=class)
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

pred_frame <- function(method, pred, data, sampling, class) {
  if (method == "majority") {
    return(cat_majority(data, pred))
  }
  else if (method == "random") {
    return(cat_rand(data,pred))
  }
  else {
    return(cat_predict(data,method,pred))
  }
}
