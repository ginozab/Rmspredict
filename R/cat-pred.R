# categorical prediction

#' FUNCTION: cat_predict
#'
#' This function takes in data
#' predicts the desired value using the
#' desired machine learning algorithm

cat_predict <- function(data, method, pred) {
  train <- data[!(data$program==pred),]
  test <- data[(data$program==pred),]
  test$program <- NULL
  train$program <- NULL
  testResults <- dplyr::select(test, source, range)
  #testEvoResults <- data.frame("program" = testEvo$program, "range" = testEvo$range)
  test$range <- NULL

  inTraining <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
  model <- train(range~., data=train, trControl=inTraining, method=method)
  predict <- predict(model, newdata = test)
  test <- dplyr::full_join(test, testResults, by="source")
  #test["range"] <- testResults$range
  #print(testEvo$range)
  cmCat <- confusionMatrix(predict, test$range)
  return(cmCat$overall["Accuracy"])
}
