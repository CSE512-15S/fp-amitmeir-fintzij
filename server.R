# server.R performs the server-side calculations for ui.R

library(glmnet)
library(ggvis)
library(graphics)
library(pROC)
library(gam)
source("InteractionPlotFunction.R")

shinyServer(function(input, output, session) { 
  # reactive expression for the dataset
  
  inputData <- reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can be found.
    inFile <- input$dataset
    
    if(!is.null(inFile)){
      dat <- read.csv(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote)
      
    } else{
      dat <- iris
      
      dat$is.virginica <- ifelse(dat$Species == "virginica", TRUE, FALSE)
      dat$Species <- NULL
      
    }
    
    return(dat)
    
  })
  
  # code for mutually exclusive selection of predictors and response
  
  # server function to render the data table          
  
  output$contents <- renderDataTable({
    
    inputData()
    
  }) 
  
  
  # server function to render the data summary
  
  output$summary <- renderPrint({
    
    dataset <- inputData()
    
    summary(dataset)
    
  })
  
  
  # selectizeInput for variable selection UI 
  
  output$response <- renderUI({
    
    input$constructionButton
    
    # get dataset
    inFile <- inputData()
    
    # extract variable names
    varnames <- names(inFile)
    
    binary.vars <- apply(inFile, 2, function(x) ifelse(length(unique(x)) == 2, T, F))
    
    varnames <- varnames[binary.vars]
    
    # generate selectizeInputs
    selectizeInput("response", "Response Variable", choices = varnames)  
    
  })
  
  variables <- reactiveValues(allVars = NULL,
                              responseVar = NULL,
                              predictorVars = NULL,
                              interactionVars = NULL,
                              varsInModel = NULL)
  
  fittedmod <- reactiveValues(fit = NULL,
                              prediction = NULL,
                              error = NULL,
                              penalty = isolate(input$lambda))
  
  
  observe({
    
    variables$allVars <- names(inputData())
    variables$responseVar <- input$response
    variables$predictorVars <- setdiff(variables$allVars, variables$responseVar)

  })
  
  output$maineffects <- renderUI({
    
    selectizeInput("maineffects", label = "Main Effects", choices = variables$predictorVars, multiple = TRUE)
    
  })
  
  observe({
    
    variables$varsInModel <- input$maineffects
    
  })
  
  
  model <- reactive({
    
    dataset <- inputData()
    responsevar <- variables$responseVar
    varsinmod <- variables$varsInModel
    penalty <- fittedmod$penalty
    
    fitGlmnetModel(response = responsevar, varsInModel = varsinmod, lambda = penalty, data = dataset)
    
  })
  
  reactive({

    fitmod <- model()
    
    fittedmod$fit <- fitmod$fit
    fittedmod$responseVar <- fitmod$prediction
    fittedmod$error <- fitmod$error
    fittedmod$penalty <- fitmod$penalty
    
  })
  
  
  output$printlambda <- renderText({
    
    fittedmod <- model()
    lambda <- isolate(fittedmod$penalty)
    lambda
    
  })
  
  # printresponse
  output$printresponse <- renderPrint({
    
    responsevar <- variables$response
    
    cat(responsevar)
    
  })
  
  output$printpreds <- renderPrint({
    
    predvars <- variables$varsInModel
    
    if(is.null(predvars)) predvars <- "None selected"
    
    cat(predvars, sep = ", ")
    
  })
  
  
  # main effects plot
#   maineffectsettings <- reactive({
#     
#     dat <- inputData()
# 
#     predictors <- variables$predictorVars
#     varsinmodel <- variables$varsInModel
#     responsevar <- variables$responseVar
#     error <- fittedmod$error  
#     
#      mainEffectPlot(allVariables = predictors,
#                     varsInModel = varsinmodel,
#                     response = responsevar,
#                     data = dat,
#                     error=error)
#     
#     
#   }) %>%  bind_shiny("mainEffectsPlot")
#   
  # interactions plot
#   reactive({
#     
#     dat <- inputData()
#     
#     varsinmodel <- variables$varsInModel
#     err <- fittedmod$error
#     
#     interactionPlot(varsInModel = varsinmodel, data = dat, error = err)
#     
#   }) %>% bind_shiny("interactionplot")
  
  
  # selectizeInput for plot margins
  output$vismargins <- renderUI({
    
    # extract variable names
    predic_vars <- variables$predictorVars
    
    # generate selectizeInputs
    list(
      selectizeInput("var1vis", "Margin 1", choices = predic_vars),
      selectizeInput("var2vis", "Margin 2", choices = predic_vars)
    )
    
  })
  
  # selction of margins for plotting
  ##
  # REACTIVE VALUES :
  S <- reactiveValues(oldvar1vis=0, oldvar2vis=0) 
  
  # REACTIVE VALUES :
  UV <- reactiveValues(var1vis=NULL,  var2vis=NULL, count=0)
  
  observe({
    if(!is.null(UV$var1vis) && !is.null(UV$var2vis)) UV$count <- isolate(UV$count) + any(UV$var1vis==UV$var2vis)
  })
  output$count <- renderText({ UV$count })
  ##
  #
  # the case of one of the var1vis set to the same value as the reponse
  #
  observe({
    if(!is.null(input$var2vis)){
      S$oldvar1vis <- isolate(input$var1vis)
    }
  })
  observe({
    if(!is.null(input$var1vis)){
      if(all(input$var1vis!=isolate(input$var2vis))){
        S$oldvar1vis <- input$var1vis
        UV$var1vis <- input$var1vis; UV$var2vis <- isolate(input$var2vis)
      }
      else{ # we exchange the matching predictor and var2vis 
        oldvar1vis <- isolate(S$oldvar1vis)
        updateSelectInput(session, "var2vis", choices=input$predictors, selected=oldvar1vis)
        UV$var1vis <- input$var1vis; UV$var2vis <- oldvar1vis 
      }
    }
  })
  ##
  #
  # 2) the case of the var2vis being set to the same value as a predictor
  #
  observe({
    if(!is.null(input$var1vis)){
      S$oldvar2vis <- isolate(input$var2vis)
    }
  })
  observe({
    if(!is.null(input$var2vis)){
      if(all(input$var2vis!=isolate(input$var1vis))){
        S$oldvar2vis <- input$var2vis
        UV$var1vis <- isolate(input$var1vis); UV$var2vis <- input$var2vis
      }
      else{  #  exchange var1vis and var2vis 
        oldvar2vis <- isolate(S$oldvar2vis)
        updateSelectInput(session, "var1vis", choices=input$predictors, selected=oldvar2vis)
        UV$var1vis <- oldvar2vis; UV$var2vis <- input$var2vis
      }
    }
  })
})
