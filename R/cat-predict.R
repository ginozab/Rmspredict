# Categorical Prediction Basic

#' FUNCTION: cat_prediction
#'
#' This function is supports basic categorical prediction
#'

cat_prediction <- function(data, method, pred, kfolds = 10, repeats = 1) {
  train <- data[!(data$program==pred),]
  test <- data[(data$program==pred),]
  test$program <- NULL
  train$program <- NULL
  testRanges <- dplyr::select(test, source, range)
  test$range <- NULL

  inTraining <- trainControl(method = "repeatedcv", number = kfolds, repeats = repeats, verbose = FALSE)
  model <- train(range~., data=train, trControl=inTraining, method=method)
  test$prediction <- predict(model, newdata = test)
  test <- dplyr::full_join(test, testRanges, by="source")
  cmCat <- confusionMatrix(test$prediction, test$range)
  return(cmCat$overall["Accuracy"])
}
