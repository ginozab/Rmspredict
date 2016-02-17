# FUNCTION: train
#
# @export

train <- function(method="rf", pred="bpmail", data=totalCDNoNEvo) {
  print(colnames(data))
  print(pred)

  trainEvo1 <- totalCDNoNEvo[!(totalCDNoNEvo$program=="bpmail"),]
  #print(trainEvo1)
  trainEvo <- data[!(data$program==pred),]
  #return(trainEvo)
}
