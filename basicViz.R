library(ggvis)
library(ggplot2)

generateMainPlot <- function(xVar=NULL,xVarName="xVar",
                             yVar=NULL,yVarName="yVar",
                             response=NULL,responseName="response",
                             numObs,
                             model=NULL,
                             facetX=NULL,facetXName="facetX",
                             facetY=NULL,facetYName="facetY") {
  #Taking care of possible null variables and creating data set
  xVarNull <- is.null(xVar)
  yVarNull <- is.null(yVar)
  facetXnull <- is.null(facetX)
  facetYnull <- is.null(facetY)
  responseNull <- is.null(response)
  
  if(xVarNull) xVar <- rep("",numObs)
  if(xVarNull) xVar <- rep("",numObs)
  if(facetXnull) facetX <- rep("",numObs)
  if(facetYnull) facetY <- rep("",numObs)
  if(responseNull) response <- rep(1,numObs)
  
  dataset <- data.frame(xVar,yVar,facetX,facetY,response)
  
  plot <- ggplot(dataset)
  
  #If both variables are null return empty plot
  if(is.null(xVar) & is.null(yVar)) {
    return(plot)
  }
  
  #If just one is null return a histogram
  if(is.null(yVar) | is.null(xVar)) {
    if(is.null(yVar)) {
      variable <- xVar
      variableName <- xVarName
    } else {
      variable <- yVar
      variableName <- yVarName
    }
    
    dataset$variable <- variable
    plot <- plot + geom_boxplot(aes(x=response,y=variable)) + xlab(variableName)
    
    #flip coordinates if x is null
    if(is.null(xVar)) {
      plot <- plot + flip_coord
    }
  
  #If both variables are not null plot scatter plot
  } else { 
    plot <- plot + geom_point(aes(x=xVar,y=yVar,color=response)) 
    plot <- plot + xlab(xVarName) + ylab(yVarName)
  }
  
  #Facet according to facetting variables 
  plot <- plot + facet_grid(facetY ~ facetX)
  
  return(plot)
}

yVar <- iris$Sepal.Width
xVar <- iris$Sepal.Length
facetX <- rbinom(length(yVar),1,0.5)
facetY <- rbinom(length(yVar),1,0.5)
response <- iris$Species=="virginica"

generateMainPlot(xVar=xVar,xVarName="Sepal Width",
                 yVar=yVar,yVarName="Sepal Length",
                 response=response,responseName="Virginica",
                 numObs=nrow(iris),
                 model=NULL,
                 facetX=facetX,facetXName="randomX",
                 facetY=facetY,facetYName="randomY")