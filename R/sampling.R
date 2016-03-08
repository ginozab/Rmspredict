# sampling

#' FUNCTION: cat_sample
#'
#' This function takes in data and samples it to the desired
#' technique
#' @param data to manipulate
#' @param target variable to manipulate data to
#' @param sampling method either up or down
#' @export

cat_sample <- function(data, sampling, target) {
  if(sampling == "up") {
    up <- data.frame(upSample(x = data, y = data[,target]))
    return(up)
  }

  else if (sampling == "down") {
    down <- data.frame(downSample(x = data, y = data[,target]))
    return(down)
  }

  else {
    stop("Invalid sampling technique: input either 'up' or 'down'")
  }
}
