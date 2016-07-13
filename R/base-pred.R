# baseline predictors

#' FUNCTION: cat_majority
#'
#' This function takes in data and the desired program to predict mutation
#' scores for and returns the accuracy when predicting majority occurrence
#' @export

cat_majority <- function(data, pred, col = "program", predict = "range") {
  train <- data[!(data[,col]==pred),]
  test <- data[(data[,col]==pred),]
  m <- tail(names(sort(table(train[,predict]))), 1)
  c <- length(which(test[,predict] == m))
  acc.major <- (c/nrow(test))
  test$predictions <- rep(m, nrow(test))
  cm <- confusionMatrix(test$predictions, test$range)
  ret <- list("acc" = cm$overall["Accuracy"], "kap" = cm$overall["Kappa"], "matrix" = cm, "data" = test)
  #return(toString(acc.major))
  return(ret)
}

#' FUNCTION: cat_rand
#'
#' This function takes in data and the desired program to predict mutation
#' scores for and returns the accuracy when predicting random occurrences
#'

cat_rand <- function(data, pred, progcol = "program", predcol = "range") {
  test <- data[(data[,progcol]==pred),]
  elems.occ <- levels(test[,predcol])
  predictions <- sample(elems.occ,1, size = nrow(test))
  preds <- data.frame(predictions)
  print(length(levels(preds)))
  test <- cbind(test, preds)
  print(length(levels(test$predictions)))
  cm <- confusionMatrix(test[,predcol], test$prediction)

  return(cm$overall["Accuracy"])
}
