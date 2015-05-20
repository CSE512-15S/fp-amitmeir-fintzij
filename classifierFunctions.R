fitClassifierLogit <- function(data,responseIndex,predictorIndices) {
  fit <- glm(data[,responseIndex]~data[,predictorIndices],family="binomial")
  return(fit)
}

getClassificationValues <- function(fit,xIndex,yIndex,data,otherPredictors,xrange,yrange) {
  averagedVariables <- dataset[]
}