interactionPlot <- function(varsInModel,data,error) {
  if(length(varsInModel)==0) return(NULL)
  
  #Computing correlations with error
  varsInModel <- sort(varsInModel)
  nInteractions <- choose(length(varsInModel),2)+length(varsInModel)
  varNameLevels <- c(varsInModel,sample(varsInModel,nInteractions-length(varsInModel),replace=TRUE))
  interactions <- data.frame(var1=varNameLevels,
                             var2=varNameLevels,
                             errorCorrelation=numeric(nInteractions))
  count <- 0
  for(i in 1:length(varsInModel)) {
    for(j in i:length(varsInModel)) {
      count <- count + 1
      generateNewVar <- paste("with(data,",varsInModel[i],"*",varsInModel[j],")")
      newVar <- eval(parse(text=generateNewVar))
      interactions[count,1:2] <- c(varsInModel[i],varsInModel[j])
      interactions[count,3] <- cor(error,newVar,method="spearman")
    }
  }
  
  #interactions[,3] <- runif(min=-1,max=1,nrow(interactions))
  
  interactions$roundCor <- round(interactions$errorCorrelation,2)
  
  ##Setting up correlations colors
  Blue = colorRampPalette(c("blue","grey"))
  Red = colorRampPalette(c("grey","red"))
  
  # Negative values of defense get a blue color scale with 10 colors
  interactions$def.color[interactions$roundCor<0] = 
    as.character(cut(interactions$roundCor[interactions$roundCor<0], 
                     seq(-1.1, 0, length.out=21), 
                     labels=Blue(20)))
  
  # Positive values of defense get an orange color scale with 10 colors
  interactions$def.color[interactions$roundCor>=0] = 
    as.character(cut(interactions$roundCor[interactions$roundCor>=0], 
                     seq(-0.1,1.1,length.out=21), 
                     labels=Red(20)))
  
  #tooltip function
  interactionToolTip <- function(x) {
    if(is.null(x)) return(NULL)
    row <- interactions[interactions$id == x$id, ]
    paste(row$var1,"X",row$var2,"\n","Error Correlation: ",round(row$errorCorrelation,2))
  }
  
  clickToolTip <- function(x) {
    if(is.null(x)) return(NULL)
    row <- interactions[interactions$id == x$id, ]
    print(c(as.character(row$var1),as.character(row$var2)))
    return(NULL)
  }
  
  interactions$id <- 1:nrow(interactions)
  interactions$errorCorRound2 <- round(interactions$errorCorrelation,2)
  
  ggvisPlot <- ggvis(data=interactions,x=~var1,y=~var2,fill:=~def.color,key:=~id) %>% 
    #layer_points() %>%
    layer_rects(width=band(),height=band(),fillOpacity:=0.8,fillOpacity.hover:=1) %>% 
    #layer_rects() %>% 
    layer_text(text:=~errorCorRound2, stroke:="black",fill:="white", align:="left", baseline:="top",fontSize:=100/length(varsInModel)) %>%
    scale_nominal("x", padding = 0, points = FALSE) %>% 
    scale_nominal("y", padding = 0, points = FALSE) %>%
    add_tooltip(interactionToolTip, "hover") %>%
    add_tooltip(clickToolTip,"click")
      
  return(ggvisPlot)
}

mainEffectPlot <- function(allVariables,varsInModel,response,data,error=NULL) {
  #Computing correlations
  nVars <- length(allVariables)
  correlations <- data.frame(variable=allVariables,correlation=nVars)
  for(i in 1:nVars) {
    if(FALSE) { #if(allVariables[i] %in% varsInModel) {
      correlations$correlation[i] <- 1
      } else {
        commandComputeCor <- "with(data,cor("
        if(is.null(varsInModel)) {
        commandComputeCor <- paste(commandComputeCor,response)
      } else { 
        commandComputeCor <- paste(commandComputeCor,"error")
      }
      commandComputeCor <- paste(commandComputeCor,",",allVariables[i],",method='spearman'))")
      correlations$correlation[i] <- eval(parse(text=commandComputeCor))
    }
  }
  
  correlations$roundCor <- round(correlations$correlation,2)
  
  Blue = colorRampPalette(c("blue","grey"))
  Red = colorRampPalette(c("grey","red"))
  black = colorRampPalette("black")
  
  # Negative values of defense get a blue color scale with 10 colors
  correlations$def.color[correlations$roundCor<0] = 
    as.character(cut(correlations$roundCor[correlations$roundCor<0], 
                     seq(-1.1, 0, length.out=21), 
                     labels=Blue(20)))
  
  # Positive values of defense get an orange color scale with 10 colors
  correlations$def.color[correlations$roundCor>=0] = 
    as.character(cut(correlations$roundCor[correlations$roundCor>=0], 
                     seq(-0.1,1.1,length.out=21), 
                     labels=Red(20)))
  
  correlations$def.color[which(allVariables %in% varsInModel)] <- "#0000"
  
  #tooltip function
  interactionToolTip <- function(x) {
    if(is.null(x)) return(NULL)
    row <- correlations[correlations$id == x$id, ]
    paste(row$variable,": ",round(row$correlation,2),sep="")
  }
  
  clickToolTip <- function(x) {
    if(is.null(x)) return(NULL)
    row <- correlations[correlations$id == x$id, ]
    print(as.character(row$variable))
    return(NULL)
  }
  
  correlations$id <- 1:nrow(correlations)
  correlations$zeros <- rep(0,nrow(correlations))
  correlations$absCorrelation <- abs(correlations$correlation) 
  
  ggvisPlot <- ggvis(data=correlations,x=~variable,y=~absCorrelation,fill:=~def.color,key:=~id) %>% 
    layer_points(size:=5000/length(allVariables),fillOpacity=0.75,fillOpacity.hover=1) %>%
    #layer_rects(width=band()) %>%
    add_tooltip(interactionToolTip, "hover") %>%
    add_tooltip(clickToolTip,"click") %>%
    layer_points(x:=0,y=1,opacity=0) #For setting axes limits

  return(ggvisPlot)
}

fitGlmnetModel <- function(response,varsInModel,data,lambda=NULL,family="binomial") {
  if(is.null(varsInModel)) return(NULL)
    
  require(glmnet)
  #Generating design matrix
  commandDesignMatrix <- "X <- model.frame(~1"
  for(i in 1:length(varsInModel)) {
    commandDesignMatrix <- paste(commandDesignMatrix,"+",varsInModel[i])
  }
  commandDesignMatrix <- paste(commandDesignMatrix,",data=data)")
  eval(parse(text=commandDesignMatrix))
  
  #Fitting Model 
  commandFitModel <- paste("fit <- cv.glmnet(y=data$",response,",x=as.matrix(X),family='",family,"')",sep="")
  eval(parse(text=commandFitModel))
  
  if(is.null(lambda)) {
    prediction <- predict(fit,newx=as.matrix(X),type="response")
  } else {
    prediction <- predict(fit,newx=as.matrix(X),type="response",s=lambda)
  }
  
  commandComputeError <- paste("error <- prediction-data$",response)
  eval(parse(text=commandComputeError))
  return(list(fit=fit,prediction=prediction,error=error,penalty=fit$lambda))
}

mainPlotFunction <- function(xVar=NULL,yVar=NULL,facetX=NULL,facetY=NULL,response=NULL,data,predictions) {
  #Constructing data set for the plot
  tempData <- data[,which(names(data) %in% c(xVar,yVar,facetX,facetY,response))]
  tempData <- data.frame(tempData,predictions)
  
  #Temporary faceting variables for testing
  ###############
  tempData <- data.frame(tempData,xfac=rbinom(nrow(data),1,0.5),yfac=rbinom(nrow(data),1,0.5))
  ################
  
  #return null if no variables are selected
  if(all(is.null(c(xVar,yVar)))) return(NULL)
  
  #If only one variable is selected then return a boxplot
  if(sum(is.null(c(xVar,yVar)))==1) {
    variable <- ifelse(!is.null(xVar),xVar,yVar)
    commandPlot <- paste("ggplot(data=tempData,aes(x=",response,",y=",variable,"))")
    commandPlot <- paste(commandPlot,"+geom_boxplot()")
    commandPlot <- paste(commandPlot,"+facet_grid(",facetY,"~",facetX,",labeller=label_both)")
    commandPlot <- paste(commandPlot,"+theme_bw()")
    if(is.null(xVar)) commandPlot <- paste(commandPlot,"+coord_flip()")
    ggPlot <- eval(parse(text=commandPlot))
    return(ggPlot)
  }
  
  #If both variables are supplied
  ## Add dummy faceting variables for streamlining
  if(is.null(facetX)) {
    tempData$xfacet <- rep("",nrow(tempData))
    facetX <- "xfacet"
  }
  
  if(is.null(facetY)) {
    tempData$yfacet <- rep("",nrow(tempData))
    facetY <- "yfacet"
  }
  
  #A function for smoothing the predictions over the domain of the variables
  #Computing the ranges on which smoothing must be done
  commandXRange <- paste("with(tempData,c(min(",xVar,"),max(",xVar,")) + sd(",xVar,")*0.1*c(-1,1))")
  xRange <- eval(parse(text=commandXRange))
  commandYRange <- paste("with(tempData,c(min(",yVar,"),max(",yVar,")) + sd(",yVar,")*0.1*c(-1,1))")
  yRange <- eval(parse(text=commandYRange))
  
  #smoothPrediction <- function(subset,xVar,yVar,facetX,facetY,xRange,yRange) {
  subset <- tempData
    #Fitting smoother to predictions
    commandSmoothFit <- paste("gam(predictions~lo(",xVar,",",yVar,"),data=subset)")
    smoothFit <- eval(parse(text=commandSmoothFit))
    #If negative residuals than the model needs to be refit as an additive model
    if(smoothFit$df.residual<0) {
      commandSmoothFit <- paste("gam(predictions~s(",xVar,")+s(",yVar,"),data=subset)")
      smoothFit <- eval(parse(text=commandSmoothFit))
    }
    
    #Setting up "new data"
    grid <- expand.grid(x=seq(from=xRange[1],to=xRange[2],length.out=150),
                        y=seq(from=yRange[1],to=yRange[2],length.out=150))
    names(grid) <- c(xVar,yVar)
    grid <- data.frame(grid)
    pred <- predict.gam(smoothFit,newdata=grid,type="response")
    pred <- pmin(pmax(pred,0),1)
    grid <- data.frame()
    
  }
  
}

# TEST
# result <- fitGlmnetModel(response,varsInModel,data,lambda=NULL)
# error <- result$error
# interactionPlot(varsInModel,data,error)










