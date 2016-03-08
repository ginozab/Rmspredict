#' FUNCTION: cpred_by_program
#'
#' Categorical prediction when splitting the data frame to
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
  cmCat <- confusionMatrix(predictMC50, testEvo$range)
  return(cmCat)
}

#' FUNCTION: npred_by_program
#'
#' Numerical prediction of Mutation Scores
#' based on source code metrics
#' @param method Machine learning algorithm
#' @param pred Program to predict mutation scores
#' @param data Dataframe with metrics and mutation scores
#' @export

npred_by_program <- function(method="rf", pred="bpmail", data=totalCDNoNEvo) {
  #print(colnames(data))
  trainEvo <- data[!(data$program==pred),]
  testEvo <- data[(data$program==pred),]

  testEvo$program <- NULL
  trainEvo$program <- NULL
  testEvoResults <- data.frame(testEvo$source, testEvo$MS)
  testEvoResults <- setNames(testEvoResults, c("source", "MS"))
  testEvo$MS <- NULL

  #print(colnames(testEvo))
  #print(colnames(testEvoResults))

  inTrainingEvo <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

  modelEvo <- train(MS ~ ., data = trainEvo, method=method, trControl=inTrainingEvo)
  testEvo$predictions <- predict(modelEvo, newdata = testEvo)

  testEvo <- merge(testEvo, testEvoResults, by="source")
  Rmspredict::graph_npreds(testEvo)
  #return(testEvo)
}



#' FUNCTION: cat_pred_bench_all
#'
#' This function runs experimentation
#' prediction on all programs using all
#' machine learning techniques
#' @export


cat_pred_bench_all <- function(trials) {
  #programs <- list("bpmail", "netweaver","diebierse","geogoogle","hftbomberman","inspirento","jnfe","jniinchi","lagoon","lavalamp","schemaspy","xisemele")
  programs <- list("bpmail", "netweaver")
  data.list <- list()
  for (i in 1:length(programs)) {
    data.list[[i]] <- Rmspredict::cat_pred_bench(trials = trials, pred = programs[[i]], data = totalCDCatEvo)
  }
  df <- do.call("rbind", data.list)
  #Rmspredict::graph_cpred(df)
  # df$value was a char so as.numeric turns it into a double
  # found this out using the df %>% dplyr::glimpse() function to
  # see the variable types
  df$value <- as.numeric(df$value)
  return(df)
}

#' FUNCTION: cat_pred_bench
#'
#' This method is for experimentation
#' runs prediction multiple times
#' @export

cat_pred_bench <- function(trials, pred, data) {
  print(pred)
  #methods <- list("gbm", "svmRadial", "parRF","C5.0", "rf")
  #methods <- list("svmRadial", "parRF", "majority")
  methods <- list("majority", "random")
  acc.output <- matrix(ncol = length(methods), nrow = trials)
  for (j in 1:length(methods)) {
    print(methods[[j]])
    for (i in 1:trials) {
      acc.output[i,j] <- Rmspredict::cat_pred(method=methods[[j]], pred=pred, data=data)
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

cat_pred <- function(method, pred, data) {
  if (method == "majority") {
    return(cat_majority(data, pred))
  }

  else if (method == "random") {
    return(cat_rand(data,pred))
  }
  else {
    trainEvo <- data[!(data$program==pred),]
    testEvo <- data[(data$program==pred),]
    #testEvo$program <- NULL
    #trainEvo$program <- NULL
    testEvoResults <- data.frame("program" = testEvo$program, "range" = testEvo$range)
    testEvo$range <- NULL

    inTraining <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
    modelFC50 <- train(range~., data=trainEvo, trControl=inTraining, method=method)
    predictMC50 <- predict(modelFC50, newdata = testEvo)
    testEvo["range"] <- testEvoResults$range
    #print(testEvo$range)
    cmCat <- confusionMatrix(predictMC50, testEvo$range)
    return(cmCat$overall["Accuracy"])
  }
}
