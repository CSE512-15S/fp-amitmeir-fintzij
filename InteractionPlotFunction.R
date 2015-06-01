interactionPlot <- function(varsInModel,data,error) {
  if(length(varsInModel==0)) return(NULL)
  
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
    layer_rects(width=band(),height=band()) %>% 
    #layer_rects() %>% 
    layer_text(text:=~errorCorRound2, stroke:="black",fill:="white", align:="left", baseline:="top",fontSize:=150/length(varsInModel)) %>%
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
    if(allVariables[i] %in% varsInModel) {
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
  
  #tooltip function
  interactionToolTip <- function(x) {
    if(is.null(x)) return(NULL)
    row <- interactions[correlations$id == x$id, ]
    paste(row$variable,": ",round(row$correlation,2),sep="")
  }
  
  clickToolTip <- function(x) {
    if(is.null(x)) return(NULL)
    row <- interactions[correlations$id == x$id, ]
    print(variable)
    return(NULL)
  }
  
  correlations$id <- 1:nrow(correlations)
  
  ggvisPlot <- ggvis(data=correlations,x=~variable,y=~correlation,fill:=~def.color,key:=~id) %>% 
    layer_bars() %>%
    add_tooltip(interactionToolTip, "hover") %>%
    add_tooltip(clickToolTip,"click")
  
  return(ggvisPlot)
  
}

fitGlmnetModel <- function(response,varsInModel,data,lambda=NULL) {
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
  commandFitModel <- paste("fit <- cv.glmnet(y=data$",response,",x=as.matrix(X),family='binomial')")
  eval(parse(text=commandFitModel))
  
  if(is.null(lambda)) {
    prediction <- predict(fit,newx=as.matrix(X),type="response")
  } else {
    prediction <- predict(fit,newx=as.matrix(X),type="response",s=lambda)
  }
  
  commandComputeError <- paste("error <- prediction-data$",response)
  eval(parse(text=commandComputeError))
  return(list(fit=fit,prediction=prediction,error=error))
}

# TEST
# result <- fitGlmnetModel(response,varsInModel,data,lambda=NULL)
# error <- result$error
# interactionPlot(varsInModel,data,error)










