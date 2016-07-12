#' FUNCTION: Numerical prediction graph
#'
#' This function takes in the results of
#' running predictions and then plots
#' the comparison of the actual mutation score
#' to the predicted mutation score
#'

graph_npreds <- function(data) {
  Numeric <- dplyr::select(data, source, MS, predictions)
  Numeric$Difference <- (abs(data$MS - data$predictions))
  Numeric <- melt(Numeric, id=c("source", "Difference"))
  Numeric$variable <- factor(Numeric$variable, levels = c("MS", "predictions"), labels = c("Actual", "Predicted"))
  colnames(Numeric)[colnames(Numeric)=="variable"] <- "Legend"

  ggplot(data=Numeric, aes(source, value, color = Legend)) +
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
    theme_bw() +
    scale_fill_grey(start=.3, end=.6) +
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


#' FUNCTION: graph_overall
#'
#' This graphing function creates a single box and whisker plot to visualize the
#' results from running the benchmark function in mspredictr
#' @export


graph_overall <- function(data, ytitle, title, x_string = "Algorithm", y_string = "value") {
  data$Trial <- NULL
  data$X <- NULL
  data$Categories <- NULL
  data$genMethod <- NULL
  data.melt <- melt(data, id=c("Program", "Algorithm"))

  ggplot(data = data.melt, aes_string(x = x_string, y = y_string)) +
    geom_boxplot(aes(fill = variable)) +
    xlab("Algorithm") +
    ylab(ytitle) +
    ggtitle(title) +
    theme_bw() +
    scale_fill_grey(start = .5, end = .8) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text = element_text(size = 16),
          axis.title = element_text(size = 18),
          legend.text = element_text(size = 16),
          legend.title = element_text(size = 18),
          plot.title = element_text(size = 18))

}


#' FUNCTION: graph_preds
#'
#' This function creates a faceted box and whisker plot, based on the program, from the results returned by the
#' benchmark function in mspredictr
#' @export

graph_preds <- function(data, ytitle, title, x_string = "Algorithm", y_string = "value") {

  data$Trial <- NULL
  data$X <- NULL
  data$Categories <- NULL
  data$genMethod <- NULL
  data.melt <- melt(data, id=c("Program", "Algorithm"))

  ggplot(data = data.melt, aes_string(x = x_string, y = y_string)) +
    geom_boxplot(aes(fill = variable)) +
    facet_wrap( ~ Program) +
    xlab("Algorithm") +
    ylab(ytitle) +
    ggtitle(title) +
    theme_bw() +
    scale_fill_grey(start = .5, end = .8) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

}


