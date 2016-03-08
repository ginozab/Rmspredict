# categorical prediction

#' FUNCTION: cat_predict
#'
#' This function takes in data
#' predicts the desired value using the
#' desired machine learning algorithm

cat_predict <- function(data, method, pred) {
  train1 <- data[!(data$program==pred),]
  print(summary(train1))
  train <- Rmspredict::cat_sample(train1, "up", "range")
  train$Class <- NULL
  print(summary(train))
  test <- data[(data$program==pred),]
  test <- Rmspredict::cat_sample(test, "up", "range")
  #test$MMLOC <- as.factor(test$MMLOC)
  #test <- cat_sample(test, "up", "MMLOC")
  #test$MMLOC <- as.numeric(test$MMLOC)
  print(summary(test))
  test$program <- NULL
  train$program <- NULL
  testRanges <- dplyr::select(test, source, range)
  #testEvoResults <- data.frame("program" = testEvo$program, "range" = testEvo$range)
  test$range <- NULL

  inTraining <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
  model <- train(range~., data=train, trControl=inTraining, method=method)
  test$prediction <- predict(model, newdata = test)
  #print(predict)
  test <- dplyr::full_join(test, testRanges, by="source")
  imp <- varImp(model, scale = FALSE)
  print(imp)
  #test["prediction"] <- predict
  #test["range"] <- testResults$range
  #print(testEvo$range)
  #cmCat <- confusionMatrix(predict, test$range)
  #return(cmCat$overall["Accuracy"])
  return(test)
}
