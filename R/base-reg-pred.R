# baseline predictions for regression
# predicts average

#' FUNCTION: reg_average_pred
#'
#' This function predicts the average mutation score
#'

reg_average_pred <- function(data, pred) {
  train <- data[!(data[,"program"]==pred),]
  test <- data[(data[,"program"]==pred),]

  test_preds <- dplyr::mutate(test, predictions = mean(train[,"MS"]))

  rmse <- RMSE(test_preds[,"predictions"], test_preds[,"MS"])
  mae <- mae(test_preds[,"predictions"], test_preds[,"MS"])
  ret <- list("rmse" = rmse, "mae" = mae, "data" = test_preds)
  return(ret)

}
