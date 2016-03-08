# Numerical Prediction

#' FUNCTION: num_predict
#'
#' This function takes in data and
#' predicts the desired value using
#' the desired machine learning algorithm
#'

num_predict <- function(data, method, pred) {
  #print(colnames(data))
  train <- data[!(data$program==pred),]
  test <- data[(data$program==pred),]

  test$program <- NULL
  train$program <- NULL
  testResults <- data.frame(test$source, test$MS)
  testResults <- setNames(testResults, c("source", "MS"))
  test$MS <- NULL

  #print(colnames(testEvo))
  #print(colnames(testEvoResults))

  inTrainingEvo <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
  model <- train(MS ~ ., data = train, method=method, trControl=inTrainingEvo)
  test$predictions <- predict(model, newdata = test)

  test <- dplyr::full_join(test, testResults, by="source")
  return(test)
}