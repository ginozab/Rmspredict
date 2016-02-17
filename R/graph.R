#' FUNCTION: Numerical prediction graph
#'
#' This function takes in the results of
#' running predictions and then plots
#' the comparison of the actual mutation score
#' to the predicted mutation score
#'

graph_npreds <- function(data) {
  Numeric <- data.frame(test$source, test$MS, test$prediction)
  Numeric$Difference <- (abs(test$MS - test$prediction))
  Numeric <- melt(Numeric, id=c("test.source", "Difference"))
  Numeric$variable <- factor(Numeric$variable, levels = c("test.MS", "test.prediction"), labels = c("Actual", "Predicted"))
  colnames(Numeric)[colnames(Numeric)=="variable"] <- "Legend"

  ggplot(data=Numeric, aes(test.source, value, color = Legend)) +
    geom_point(alpha=1, aes(size = Difference)) +
    labs(x="Program", y="Mutation Score") +
    ggtitle("Actual versus Predicted Mutation Scores") +
    scale_size_continuous(range = c(3, 9)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}
