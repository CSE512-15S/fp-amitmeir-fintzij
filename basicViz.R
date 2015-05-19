library(ggvis)
library(ggplot2)
library(pcf.kernesti)

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
                 predict=predictions,
                 facetX=facetX,facetXName="randomX",
                 facetY=facetY,facetYName="randomY")

model <- glm(response ~ iris[,1] + I(iris[,4]^2),family="binomial")
predictions <- predict(model,type="response")

predSmoother <- gam(predictions~s(xVar,yVar))
grid <- expand.grid(x=seq(from=min(xVar)-0.1*sd(xVar),to=max(xVar)+0.1*sd(xVar),by=sd(xVar)*0.1),
                    y=seq(from=min(yVar)-0.1*sd(yVar),to=max(yVar)+0.1*sd(yVar),by=sd(yVar)*0.1))
smoothed.pred <- predict(predSmoother,newdata=data.frame(xVar=grid[,1],yVar=grid[,2]))

ggplot() + geom_tile(aes(x=grid[,1],y=grid[,2],fill=as.vector(smoothed.pred))) + 
  scale_fill_gradient2(midpoint=0.5,mid="white",high="blue",low="orange") +
  geom_point(aes(x=xVar,y=yVar,color=response)) + theme_bw()

  
  