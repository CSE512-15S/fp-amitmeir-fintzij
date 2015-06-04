generateMainPlot <- function(xVar=NULL,
                             yVar=NULL,
                             response=NULL,
                             data) {
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
      
  #If both variables are null return empty plot
  if(is.null(xVar) & is.null(yVar)) {
    return(plot)
  }
  
  #If just one is null return a histogram
  if(is.null(yVar) | is.null(xVar)) {
    if(is.null(yVar)) {
      variable <- xVar
    } else {
      variable <- yVar
    }
    
    plot <- ggvis(data=data,x=parse(text=paste("~",variable)),y=parse(text=(paste("~as.factor(",response,")")))) %>%
      layer_boxplots() 
    
    #flip coordinates if x is null
  
  #If both variables are not null plot scatter plot
  } else { 
    plotCommand <- paste("ggvis(data=data,x=~",xVar,",y=~",yVar,",fill=~",response,")")
    plotCommand <- paste(plotCommand,"%>% layer_points()")
    eval(parse(text=plotCommand))
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

model <- glm(response ~ Sepal.Length + Sepal.Width,family="binomial",data=data)
predictions <- predict(model,type="response")

predSmoother <- gam(predictions~s(Sepal.Length)+ s(Sepal.Width),data=data)
grid <- with(data,expand.grid(x=seq(from=min(Sepal.Length)-0.1*sd(Sepal.Length),to=max(Sepal.Length)+0.1*sd(Sepal.Length),by=sd(Sepal.Length)*0.05),
                    y=seq(from=min(Sepal.Width)-0.1*sd(Sepal.Width),to=max(Sepal.Width)+0.1*sd(Sepal.Width),by=sd(Sepal.Width)*0.05)))
smoothed.pred <- predict(predSmoother,newdata=data.frame(Sepal.Length=grid[,1],Sepal.Width=grid[,2]))

predictorData <- data.frame(Sepal.Length=grid[,1],Sepal.Width=grid[,2],smoothed.pred)

ggvis(data=predictorData,x=~Sepal.Length,y=~Sepal.Width,fill=~smoothed.pred,opacity:=0.55,size:=20) %>% layer_points()

ggplot() + geom_tile(aes(x=grid[,1],y=grid[,2],fill=as.vector(smoothed.pred))) + 
  scale_fill_gradient2(midpoint=0.5,mid="white",high="blue",low="orange") +
  geom_point(aes(x=xVar,y=yVar,color=response)) + theme_bw()

  
  