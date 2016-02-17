#' FUNCTION: cpred_by_program
#'
#' User prediction when splitting the data frame to
#' predict for one program and train on all the rest
#' @param method Machine learning algorithm
#' @param pred Program to predict MS
#' @param data All programs with MS
#' @export

cpred_by_program <- function(method = "C5.0", pred="bpmail", data=totalCDCatEvo) {
  print(colnames(totalCDCatEvo))
  trainEvo <- data[!(data$program==pred),]

  testEvo <- data[(data$program==pred),]

  summary(testEvo)
  summary(trainEvo)

  testEvo$program <- NULL
  trainEvo$program <- NULL
  testEvo$genMethod <- NULL
  trainEvo$genMethod <- NULL

  inTraining <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

  modelFC50 <- train(range~., data=trainEvo, trControl=inTraining, method=method, preProcess=c("scale","center"))

  predictMC50 <- predict(modelFC50, newdata = testEvo[,1:22])
  print("Predicted mutation score results for 'bpmail'")
  confusionMatrix(predictMC50, testEvo$range)
}

npred_by_program <- function(method="rf", pred="bpmail", data=totalCDNoNEvo) {
  data$X.1 <- NULL
  data$X <- NULL
  data$genMethod <- NULL
  data$killed <- NULL
  data$total <- NULL

  print(colnames(data))
  trainEvo <- data[!(data$program==pred),]
  testEvo <- data[(data$program==pred),]

  testEvo$program <- NULL
  trainEvo$program <- NULL
  testEvoResults <- data.frame(testEvo$source, testEvo$MS)
  testEvoResults <- setNames(testEvoResults, c("source", "MS"))
  testEvo$MS <- NULL

  print(colnames(testEvo))
  print(colnames(testEvoResults))

  inTrainingEvo <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

  modelEvo <- train(MS ~ ., data = trainEvo, method=method, trControl=inTrainingEvo)
  testEvo$predictions <- predict(modelEvo, newdata = testEvo)

  testEvo <- merge(testEvo, testEvoResults, by="source")
  #Rmspredict::graph_npreds(testEvo)
  return(testEvo)
}

