#' FUNCTION: Numerical prediction graph
#'
#' This function takes in the results of
#' running predictions and then plots
#' the comparison of the actual mutation score
#' to the predicted mutation score
#'

graph_npreds <- function(data) {
  Numeric <- dplyr::select(data, source, MS, prediction)
  Numeric$Difference <- (abs(data$MS - data$prediction))
  Numeric <- melt(Numeric, id=c("source", "Difference"))
  Numeric$variable <- factor(Numeric$variable, levels = c("MS", "prediction"), labels = c("Actual", "Predicted"))
  colnames(Numeric)[colnames(Numeric)=="variable"] <- "Legend"

  ggplot(data=Numeric, aes(data.source, value, color = Legend)) +
    geom_point(alpha=1, aes(size = Difference)) +
    labs(x="Program", y="Mutation Score") +
    ggtitle("Actual versus Predicted Mutation Scores") +
    scale_size_continuous(range = c(3, 9)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}

#' FUNCTION: Confusion matrix graph
#'
#' This function takes in a confusion matrix
#' and creates a heatmap of the confusion matrix
#' results.
#'

graph_cm <- function(cm) {

  df <- as.data.frame(as.table(cm$table))
  ggplot(data = df, aes(x=Prediction, y=Reference, fill=Freq)) +
    geom_tile()

}

graph_cpred <- function(data) {
  ggplot(data, aes(x = variable, y=value, fill=variable)) +
    geom_boxplot() +
    facet_wrap(~ Program) +
    xlab("Program") +
    ylab("Accuracy") +
    ggtitle("Accuracy of Different Techniques") +
    scale_fill_grey() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="none")
}
