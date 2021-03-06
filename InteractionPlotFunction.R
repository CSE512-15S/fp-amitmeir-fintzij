require(pROC)

#A function for creating the interaction interface plot
interactionPlot <- function(varsInModel,data,error) {
  #   #### GGVIS COMMAND
  #   if(length(varsInModel)==0) {
  #     stupidData <- data.frame(a=1:3,b=1:3)
  #     stupidGGVIS <- ggvis(data=stupidData,x=~a,y=~b,opacity=0) %>% layer_points()  %>%
  #       set_options(keep_aspect=TRUE,resizable=TRUE)
  #     return(stupidGGVIS)
  #   }
  
  if(length(varsInModel)==0) {
    stupidData <- data.frame(a=1:3,b=1:3)
    stupidGGplot <- ggplot(stupidData) + geom_point(aes(x=a,y=b)) + theme_bw()
    return(stupidGGplot)
  }
  
  # Identifty which variable is an interaction and which is a main effect
  interactionInd <- sapply(varsInModel,function(x) grepl(":",x))
  interactions <- varsInModel[which(interactionInd)]
  varsInModel <- varsInModel[which(!interactionInd)]
  interactionMatrix <- sapply(interactions,function(x) strsplit(x,":")[[1]]) 
  interactionMatrix <- matrix(interactionMatrix,ncol=2)
  
  
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
      commandComputeR2 <- paste("summary(lm(error~",varsInModel[i],"*",varsInModel[j],",data=data))$r.squared")
      interactions[count,3] <- eval(parse(text=commandComputeR2))
    }
  }
  
  #interactions[,3] <- runif(min=0,max=1,nrow(interactions))
  
  interactions$roundCor <- round(interactions$errorCorrelation,2)
  interactions$errorCorRound2 <- round(interactions$errorCorrelation,2)
  
  #finding out which interactions are in the model 
  checkIfInInteractions <- function(row,interactionMatrix) {
    row <- sapply(row,as.character)
    for(i in 1:nrow(interactionMatrix)) {
      rowMat <- interactionMatrix[i,]
      if(row[1] %in% rowMat) {
        rowMat <- rowMat[-which(row[1]==rowMat)[[1]]]
        if(row[2]==rowMat) {
          return(TRUE)
        }
      }
    }
    return(FALSE)
  }
  if(nrow(interactionMatrix)>0) {
    inModel <- apply(interactions[,1:2],1,checkIfInInteractions,interactionMatrix)
  } else {
    inModel <- rep(FALSE,nrow(interactions))
  }
  
  interactionPlot <- ggplot(interactions) 
  if(nrow(interactionMatrix)>0) {
    interactionPlot <- interactionPlot + 
      geom_tile(data=interactions[inModel,],aes(x=var1,y=var2,height=0.98,width=0.98),fill="black") 
  } 
  interactionPlot <- interactionPlot + 
    geom_tile(aes(x=var1,y=var2,fill=errorCorrelation,height=0.88,width=0.88)) +
    theme_bw() + 
    scale_fill_gradient2(limits=c(0,1),low="blue", high="red") +
    geom_text(aes(x=var1,y=var2,label=paste(var1,"\n",var2,"\n",errorCorRound2)),size=10/length(varsInModel)) +
    scale_x_discrete(expand=c(0.04,0.04)) + 
    scale_y_discrete(expand=c(0.04,0.04)) + 
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text = element_blank()) 
  
  return(interactionPlot)
  
  ############## GGVIS Part ##############
  #   
  #   ##Setting up correlations colors
  #   Blue = colorRampPalette(c("blue","grey"))
  #   Red = colorRampPalette(c("grey","red"))
  #   
  #   # Negative values of defense get a blue color scale with 10 colors
  #   interactions$def.color[interactions$roundCor<0] = 
  #     as.character(cut(interactions$roundCor[interactions$roundCor<0], 
  #                      seq(-1.1, 0, length.out=21), 
  #                      labels=Blue(20)))
  #   
  #   # Positive values of defense get an orange color scale with 10 colors
  #   interactions$def.color[interactions$roundCor>=0] = 
  #     as.character(cut(interactions$roundCor[interactions$roundCor>=0], 
  #                      seq(-0.1,1.1,length.out=21), 
  #                      labels=Red(20)))
  #   
  #   #tooltip function
  #   interactionToolTip <- function(x) {
  #     if(is.null(x)) return(NULL)
  #     row <- interactions[interactions$id == x$id, ]
  #     paste(row$var1,"X",row$var2,"\n","Error Correlation: ",round(row$errorCorrelation,2))
  #   }
  #   
  #   clickToolTip <- function(x) {
  #     if(is.null(x)) return(NULL)
  #     row <- interactions[interactions$id == x$id,1:2]
  #     return(row)
  #   }
  #   
  #   interactions$id <- 1:nrow(interactions)
  #   
  #   ggvisPlot <- ggvis(data=interactions,x=~var1,y=~var2,fill:=~def.color,key:=~id) %>% 
  #     layer_rects(width=band(),height=band(),fillOpacity:=0.8,fillOpacity.hover:=1) %>% 
  #     layer_text(text:=~errorCorRound2, stroke:="black",fill:="white", align:="left", baseline:="top",fontSize:=100/length(varsInModel)) %>%
  #     scale_nominal("x", padding = 0, points = FALSE) %>% 
  #     scale_nominal("y", padding = 0, points = FALSE) %>%
  #     add_tooltip(interactionToolTip, "hover") %>%
  #     add_tooltip(clickToolTip,"click") %>%
  #     bind_shiny("ggvisInteraction")
  #   
  #   ggvisPlot
  #   return(ggvisPlot)
}

#A function for creating the main effect interface plot
mainEffectPlot <- function(allVariables,varsInModel,response,data,error=NULL) {
  ### Stupid GGVIS
  #   if(is.null(allVariables)) {
  #     stupidData <- data.frame(a=1:3,b=1:3)
  #     stupidGGVIS <- ggvis(data=stupidData,x=~a,y=~b,opacity=0) %>% 
  #       layer_points() %>%
  #       set_options(height = 100, keep_aspect=TRUE,resizable=TRUE)  %>%
  #       bind_shiny("ggvisMainEffect")
  #     return(stupidGGVIS)
  #   }
  
  #Stupid ggplot
  if(is.null(allVariables)) {
    stupidData <- data.frame(a=1:3,b=1:3)
    stupidGGPLOT <- ggplot(data=stupidData,aes(x=a,y=b),alpha=0)  +
      geom_point() + theme_bw()
    return(stupidGGPLOT)
    stupidGGVIS <- ggvis(data=stupidData,x=~a,y=~b,opacity=0) %>% 
      layer_points() %>%
      set_options(height = 100, keep_aspect=TRUE,resizable=TRUE)
    return(stupidGGVIS)
  }
  
  if(length(varsInModel)>0) {
    interactionInd <- sapply(varsInModel,function(x) grepl(":",x))
    interactions <- varsInModel[which(interactionInd)]
    main <- varsInModel[which(!interactionInd)]
  } else {
    main <- c()
  }
  
  
  #Computing correlations
  nVars <- length(allVariables)
  correlations <- data.frame(variable=allVariables,correlation=rep(nVars,nVars))
  for(i in 1:nVars) {
    if(FALSE) { #if(allVariables[i] %in% varsInModel) {
      correlations$correlation[i] <- 1
    } else {
      tocor <- ifelse(is.null(varsInModel),response,"error")
      commandComputeCor <- paste("summary(lm(",tocor,"~",allVariables[i],",data=data))$r.squared")
      correlations$correlation[i] <- eval(parse(text=commandComputeCor))
    }
  }
  
  correlations$roundCor <- round(correlations$correlation,2)
  if(length(varsInModel)>0) {
    inModel <- correlations$variable %in% main
  } else {
    inModel <- rep(FALSE,length(allVariables))
  }

  mainEffectPlot <- ggplot(correlations) +
    geom_point(data=correlations[inModel,],aes(x=variable,y=correlation),shape=23,fill="black",size=65/length(allVariables)) + 
    geom_point(aes(x=variable,y=correlation,fill=correlation),shape=23,size=50/length(allVariables)) + 
    theme_bw() + scale_y_continuous(limits=c(-1,1)) + 
    geom_hline(x=0) + 
    scale_fill_gradient2(low="blue",high="red",limits=c(0,1))+
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(angle = -45, vjust = 0.8, hjust = 0.05))
  
  return(mainEffectPlot)
  
  
  ####### GGVIS COMMANDS
  #   Blue = colorRampPalette(c("blue","grey"))
  #   Red = colorRampPalette(c("grey","red"))
  #   black = colorRampPalette("black")
  #   
  #   # Negative values of defense get a blue color scale with 10 colors
  #   correlations$def.color[correlations$roundCor<0] = 
  #     as.character(cut(correlations$roundCor[correlations$roundCor<0], 
  #                      seq(-1.1, 0, length.out=21), 
  #                      labels=Blue(20)))
  #   
  #   # Positive values of defense get an orange color scale with 10 colors
  #   correlations$def.color[correlations$roundCor>=0] = 
  #     as.character(cut(correlations$roundCor[correlations$roundCor>=0], 
  #                      seq(-0.1,1.1,length.out=21), 
  #                      labels=Red(20)))
  #   
  #   correlations$def.color[which(allVariables %in% varsInModel)] <- "#0000"
  #   
  #   #tooltip function
  #   interactionToolTip <- function(x) {
  #     if(is.null(x)) return(NULL)
  #     row <- correlations[correlations$id == x$id, ]
  #     paste(row$variable,": ",round(row$correlation,2),sep="")
  #   }
  #   
  #   clickToolTip <- function(x) {
  #     if(is.null(x)) return(NULL)
  #     row <- correlations[correlations$id == x$id, ]
  #     print(as.character(row$variable))
  #     return(NULL)
  #   }
  #   
  #   correlations$id <- 1:nrow(correlations)
  #   correlations$zeros <- rep(0,nrow(correlations))
  #   correlations$absCorrelation <- correlations$correlation 
  #   
  #   ggvisPlot <- ggvis(data=correlations,x=~variable,y=~absCorrelation,fill:=~def.color,key:=~id) %>% 
  #     layer_points(size:=1000/length(allVariables), shape := "diamond", fillOpacity=0.75,fillOpacity.hover=1) %>%
  #     add_axis("x", title = "Variable") %>%
  #     add_axis("y", title = "Correlation") %>%
  #     scale_numeric("y", domain = c(-1, 1), nice = TRUE) %>%
  #     #layer_rects(width=band()) %>%
  #     add_tooltip(interactionToolTip, "hover") %>%
  #     add_tooltip(clickToolTip,"click") %>%
  #     layer_points(x:=0,y=1,opacity=0) %>% #For setting axes limits
  #     set_options(height = 200, keep_aspect=TRUE, resizable=TRUE) %>%
  #     bind_shiny("ggvisMainEffect")
  #   
  #   return(ggvisPlot)

}

#A function for fitting a glmnet model
fitGlmnetModel <- function(response,varsInModel,data,lambda=NULL,family="binomial") {
  #If not predictors fit intercept
  if(is.null(varsInModel)) {
    commandFitIntercept <- paste("glm(",response,"~ 1,family=",family,",data=data)")
    fit <- eval(parse(text=commandFitIntercept))
    predictions <- predict(fit,type="response")
    commandErrors <- paste("with(data,predictions-",response,")")
    errors <- eval(parse(text=commandErrors))
    lambda <- 0
    return(list(fit=fit,prediction=predictions,error=errors,penalty=lambda,optimal=lambda))
  }
  
  #If only one predictor don't regularize
  if(length(varsInModel)==1) {
    commandFitIntercept <- paste("glm(",response,"~ ",varsInModel[1],",family=",family,",data=data)")
    fit <- eval(parse(text=commandFitIntercept))
    predictions <- predict(fit,type="response")
    commandErrors <- paste("with(data,predictions-",response,")")
    errors <- eval(parse(text=commandErrors))
    lambda <- 0
    return(list(fit=fit,prediction=predictions,error=errors,penalty=lambda,optimal=lambda))
  }
  
  # Identifty which variable is an interaction and which is a main effect
  interactionInd <- sapply(varsInModel,function(x) grepl(":",x))
  interactions <- varsInModel[which(interactionInd)]
  main <- varsInModel[which(!interactionInd)]
  interactionMatrix <- sapply(interactions,function(x) strsplit(x,":")[[1]]) 
  interactionMatrix <- matrix(interactionMatrix,ncol=2)
  
  require(glmnet)
  #Generating design matrix
  commandDesignMatrix <- paste("X <- model.matrix(lm(",response,"~1")
  for(i in 1:length(main)) {
    commandDesignMatrix <- paste(commandDesignMatrix,"+",main[i])
  }
  
  if(any(interactionInd)) {
    for(i in 1:nrow(interactionMatrix)) {
      commandDesignMatrix <- paste(commandDesignMatrix,"+I(",
                                   interactionMatrix[i,1],"*",interactionMatrix[i,2],")")
    }
  }

  commandDesignMatrix <- paste(commandDesignMatrix,",data=data))")
  eval(parse(text=commandDesignMatrix))
  
  commandFitModel <- paste("fit <- cv.glmnet(y=data$",response,",x=as.matrix(X),family='",family,"')",sep="")
  eval(parse(text=commandFitModel))
  
  if(is.null(lambda)) {
    prediction <- predict(fit,newx=as.matrix(X),type="response")
  } else {
    prediction <- predict(fit,newx=as.matrix(X),type="response",s=lambda)
  }
  
  commandComputeError <- paste("error <- prediction-data$",response)
  eval(parse(text=commandComputeError))
  return(list(fit=fit,prediction=prediction,error=error,penalty=fit$lambda,optimal=fit$lambda.min))
}

#A function for the main model fit plot
mainPlotFunction <- function(xVar="",yVar="",facetX="",facetY="",response="",data,predictions) {
  if(all(c(xVar,yVar)=="")) return(plot(1,1,xlim=c(0,0.5)))
  
  #Constructing data set for the plot
  tempData <- data[,which(names(data) %in% c(xVar,yVar,facetX,facetY,response))]
  tempData <- cbind(tempData,predictions=predictions)
  names(tempData)[ncol(tempData)] <- "predictions"
  #convert response to boolean
  commandBoolean <- paste("tempData$",response," <- tempData$",response,"==max(tempData$",response,")") 
  eval(parse(text=commandBoolean))
  
  
  #Temporary faceting variables for testing
  ###############
  #tempData <- data.frame(tempData,xfac=rbinom(nrow(data),1,0.5),yfac=rbinom(nrow(data),1,0.5))
  ################
  
  #return null if no variables are selected
  
  ## Add dummy faceting variables for streamlining
  if(facetX=="") {
    tempData$xfacet <- rep("",nrow(tempData))
    facetX <- "xfacet"
  }
  
  if(facetY=="") {
    tempData$yfacet <- rep("",nrow(tempData))
    facetY <- "yfacet"
  }
  
  ##If facetting variable has too many value, 
  facetToFactor <- function(var) {
    quantiles <- quantile(var,probs=seq(from=0,to=1,by=0.2))
    #factor names
    factorNames <- character(length(quantiles)-1) 
    for(i in length(factorNames):1) factorNames[i] <- paste(round(quantiles[i],1)," to ",round(quantiles[i+1],1),sep="")
    newVar <- factor(sample(factorNames,length(var),replace=TRUE)) #Creating new variables with correct factors
    for(i in 1:length(factorNames)) newVar[which(var>=quantiles[i])] <- factorNames[i]
    return(newVar)
  }  
  
  commandXvals <- paste("tooManyValsX <- with(tempData,is.numeric(",facetX,") & length(unique(",facetX,"))>5)")
  eval(parse(text=commandXvals))
  if(tooManyValsX) {
    commandConvertToFactor <- paste("tempData$",facetX,"<- facetToFactor(tempData$",facetX,")")
    eval(parse(text=commandConvertToFactor))
  }
  commandYvals <- paste("tooManyValsY <- with(tempData,is.numeric(",facetY,") & length(unique(",facetY,"))>5)")
  eval(parse(text=commandYvals))
  if(tooManyValsY) {
    commandConvertToFactor <- paste("tempData$",facetY,"<- facetToFactor(tempData$",facetY,")")
    eval(parse(text=commandConvertToFactor))
  }
  
  #If only one variable is selected then return a boxplot
  if(sum(c(xVar,yVar)=="")==1) {
    variable <- ifelse(xVar!="",xVar,yVar)
    commandPlot <- paste("ggplot(data=tempData,aes(x=",response,",y=",variable,"))")
    commandPlot <- paste(commandPlot,"+geom_boxplot()")
    commandPlot <- paste(commandPlot,"+facet_grid(",facetY,"~",facetX,",labeller=label_both)")
    commandPlot <- paste(commandPlot,"+theme_bw()")
    if(xVar=="") commandPlot <- paste(commandPlot,"+coord_flip()")
    ggPlot <- eval(parse(text=commandPlot))
    return(ggPlot)
  }
  
  #A function for smoothing the predictions over the domain of the variables
  #Computing the ranges on which smoothing must be done
  commandXRange <- paste("with(tempData,c(min(",xVar,"),max(",xVar,")) + sd(",xVar,")*0.1*c(-1,1))")
  xRange <- eval(parse(text=commandXRange))
  commandYRange <- paste("with(tempData,c(min(",yVar,"),max(",yVar,")) + sd(",yVar,")*0.1*c(-1,1))")
  yRange <- eval(parse(text=commandYRange))
  
  #The facetting function
  smoothPrediction <- function(rowIndices,data,xVar,yVar,facetX,facetY,xRange,yRange) {
    #Fitting smoother to predictions
    subset <- data[rowIndices,]
    commandSmoothFit <- paste("gam(predictions~lo(",xVar,",",yVar,"),data=subset)")
    smoothFit <- eval(parse(text=commandSmoothFit))
    #If negative residuals than the model needs to be refit as an additive model
    if(smoothFit$df.residual<0) {
      commandSmoothFit <- paste("gam(predictions~s(",xVar,")+s(",yVar,"),data=subset)")
      smoothFit <- eval(parse(text=commandSmoothFit))
    }
    
    #Setting up "new data"
    grid <- expand.grid(x=seq(from=xRange[1],to=xRange[2],length.out=15),
                        y=seq(from=yRange[1],to=yRange[2],length.out=15))
    names(grid) <- c(xVar,yVar)
    grid <- data.frame(grid)
    pred <- predict.gam(smoothFit,newdata=grid,type="response")
    pred <- pmin(pmax(pred,0),1)
    commandConstructGrid <- paste("data.frame(grid,predictions=pred,",
                                  response,"=rep(NA,nrow(grid)),",
                                  facetX,"=rep(subset$",facetX,"[1],nrow(grid)),",
                                  facetY,"=rep(subset$",facetY,"[1],nrow(grid)))")
    grid <- eval(parse(text=commandConstructGrid))
    return(grid)
  }
  
  #Computing the predictions according to facets
  commandSmoothedPredictions <- paste("tapply(1:nrow(tempData),INDEX=list(tempData$",
                                      facetX,",tempData$",facetY,
                                      "),smoothPrediction,data=tempData,",
                                      "xVar,yVar,facetX,facetY,xRange,yRange,simplify=FALSE)")
  predictionFacets <- eval(parse(text=commandSmoothedPredictions))
  #Joining with temporary data set
  predictionFacets <- do.call("rbind",lapply(predictionFacets,function(x) x))
  #tempData$predictions <- rep(NA,nrow(tempData))
  #tempData <- rbind(tempData,predictionFacets)
  
  #Generating "correctly.classified"
  classifyTrue <- predictions > 0.5
  responseVals <- paste("data$",response)
  responseVals <- eval(parse(text=(responseVals)))
  tempData$classification.error <- !((classifyTrue & (response>0.5)) | (!classifyTrue & (response<0.5)))
 
  #Plotting
  commandPlot <- paste("ggplot()")
  commandPlot <- paste(commandPlot,"+ geom_tile(data=predictionFacets,aes(x=",xVar,",y=",yVar,",fill=predictions),alpha=0.3)")
  commandPlot <- paste(commandPlot,"+ geom_point(data=tempData,aes(x=,",xVar,",y=",yVar,",color=",response,",shape=classification.error))")
  commandPlot <- paste(commandPlot,"+ facet_grid(",facetX,"~",facetY,",labeller='label_both')")
  commandPlot <- paste(commandPlot,"+ scale_fill_gradient2(midpoint=0.5,mid='white',high='blue',low='red')")
  commandPlot <- paste(commandPlot,"+ scale_color_manual(values=c('red3','navy'))")
  commandPlot <- paste(commandPlot,"+ scale_x_continuous(expand=c(0,0))")
  commandPlot <- paste(commandPlot,"+ scale_y_continuous(expand=c(0,0))")
  commandPlot <- paste(commandPlot,"+ guides(fill = guide_legend(override.aes = list(alpha = 0.3)))")
  commandPlot <- paste(commandPlot,"+ theme_bw()")
  
  ggPlot <- eval(parse(text=commandPlot))
  
  return(ggPlot)
}

#A function for plotting the ROC.
plotROC <- function(response,predictions,data) {
  require(pROC)
  commandROC <- paste("with(data,roc(",response,"~predictions))")
  rocObject <- eval(parse(text=commandROC))
  return(plot(smooth(rocObject),main=paste("Area Under the Curve:",rocObject$auc)))
}

#A function for plotting the cross validation plot 
plotCV <- function(fit) {
  if(is.null(fit)) return(NULL)
  return(plot(fit))
}

# #TEST
# result <- fitGlmnetModel(response,varsInModel,data,lambda=NULL,family='binomial')
# fit <- result$fit
# error <- result$error
# predictions <- result$prediction
# interactionPlot(varsInModel,data,error)
# mainEffectPlot(allVariables,varsInModel,response,data,error=error)
# data$facx <- rbinom(nrow(data),1,0.5)
# data$facy <- rbinom(nrow(data),1,0.5)
#mainPlotFunction(xVar="Sepal.Length",yVar="Petal.Width",facetX="Sepal.Width",facetY=NULL,response="is.virginica",data,predictions)
# 
# plotROC(response,predictions,data)

# result <- fitGlmnetModel(response,varsInModel,adult,lambda=NULL,family='binomial')
# fit <- result$fit
# error <- result$error
# predictions <- result$prediction
# interactionPlot(varsInModel,data,error)
# mainEffectPlot(allVariables,varsInModel,response,data,error=error)
# data$facx <- rbinom(nrow(data),1,0.5)
# data$facy <- rbinom(nrow(data),1,0.5)
# mainPlotFunction(xVar="Sepal.Length",yVar="Petal.Length",facetX="Sepal.Width",facetY=NULL,response="is.virginica",data=data,predictions=predictions)

# # 
# par(mfrow=c(1,2),mar=rep(4,4))
# plotROC(response,predictions,data)
# plot(fit)

