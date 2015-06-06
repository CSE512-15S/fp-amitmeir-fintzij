# server.R performs the server-side calculations for ui.R

library(glmnet)
library(ggplot2)
library(graphics)
library(pROC)
library(gam)
source("InteractionPlotFunction.R")

shinyServer(function(input, output, session) { 
  
  # data upload tab stuff
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
    
    # get dataset
    inFile <- inputData()
    
    # extract variable names
    varnames <- names(inFile)
    
    binary.vars <- apply(inFile, 2, function(x) ifelse(length(unique(x)) == 2, T, F))
    
    varnames <- varnames[binary.vars]
    
    # generate selectizeInputs
    selectizeInput("response", "Response Variable", choices = varnames)  
    
  })
  
  # Construction tab stuff
  variables <- reactiveValues(allVars = NULL,
                              responseVar = NULL,
                              predictorVars = NULL,
                              varsInModel = NULL)
  
  fittedmod <- reactiveValues(fit = NULL,
                              prediction = NULL,
                              error = NULL,
                              penalty = isolate(input$lambda))
  
  # observe dataset, allVars, response, and predictors
  observe({
    
    variables$allVars <- names(inputData())

    variables$responseVar <- input$response
    variables$predictorVars <- setdiff(variables$allVars, variables$responseVar)

  })
  
  # selectizeInput for main effects
  output$maineffects <- renderUI({
    
    selectizeInput("maineffects", label = "Main Effects", choices = variables$predictorVars, multiple = TRUE)
    
  })
  
  output$interactions <- renderUI({
    
    selectizeInput("interactions", label = "Interactions", choices = NULL, selected = NULL, multiple = TRUE)
    
  })
  
  observe({
    
    maineffectsInMod <- input$maineffects
    
    interactionChoices <- rep(NA, length(maineffectsInMod) + choose(length(maineffectsInMod), 2))
    
    n <- 0
    
    for(k in 1:length(maineffectsInMod)){
      for(j in k:length(maineffectsInMod)){
        
        n <- n+1
        
        interactionChoices[n] <- paste(maineffectsInMod[k],maineffectsInMod[j],sep=":")
      }
    }
    
    updateSelectizeInput(session, "interactions", choices = interactionChoices)
    
  })
  
  observe({
    
    variables$varsInModel <- c(input$maineffects, input$interactions)
    
  })
  
  
  model <- eventReactive(input$fitButton, {
    
    dataset <- inputData()
    
    responsevar <- variables$responseVar
    varsinmod <- variables$varsInModel
    penalty <- input$penalty
    
    fitGlmnetModel(response = responsevar, varsInModel = varsinmod, lambda = penalty, data = dataset)
    
  })
  
  fitreactive <- reactive({

    fitmod <- model()
    
    fittedmod$fit <- fitmod$fit
    fittedmod$prediction <- fitmod$prediction
    fittedmod$error <- fitmod$error
    fittedmod$penalty <- fitmod$penalty

  })
  

  # main effects plot
  output$mainEffectsPlot <- renderPlot({
    
    fitreactive()
    
    predictors <- variables$predictorVars
    varsinmodel <- variables$varsInModel
    responsevar <- variables$responseVar
    error <- fittedmod$error
    
    mainEffectPlot(allVariables = predictors,
                   varsInModel = varsinmodel,
                   response = responsevar,
                   data = dat,
                   error=error)
    
  })
  
  output$interactionplot <- renderPlot({
    
    fitreactive()
    
    varsinmodel <- variables$varsInModel
    error <- fittedmod$error
    
    interactionPlot(varsInModel = varsinmodel,
                   data = dat,
                   error=error)
    
  })
  

  
  output$printlambda <- renderText({
    
    fittedmod <- model()
    lambda <- isolate(fittedmod$error)
    lambda
    
  })
  
  # printresponse
  output$printresponse <- renderPrint({
    
    responsevar <- isolate(variables$responseVar)
    
    cat(responsevar)
    
  })
  
  output$printpreds <- renderPrint({
    
    predvars <- variables$varsInModel
    
    if(is.null(predvars)) predvars <- "None selected"
    
    cat(predvars, sep = ", ")
    
  })
  
  

  
  # selectizeInput for plot margins
  output$vismargins <- renderUI({
    
    # extract variable names
    predic_vars <- variables$predictorVars
    
    # generate selectizeInputs
    list(
      
      selectizeInput("var1vis", "Margin 1", choices = c("Select margin 1", predic_vars)),
      selectizeInput("var2vis", "Margin 2", choices = c("Select margin 2", predic_vars)),
      selectizeInput("facet1", "Facet 1", choices = c("Select facet 1", predic_vars)),
      selectizeInput("facet2", "Facet 2", choices = c("Select facet 2", predic_vars))
      
    )
    
  })
  
  observe({
    
    currentvar1 <- input$var1vis
    
    var2 <- input$var2vis
    facet1 <- input$facet1
    facet2 <- input$facet2
    
    updateSelectizeInput(session, "var1vis", choices = c(setdiff(variables$predictorVars, c(var2, facet1, facet2))), selected = currentvar1)
    
  })
  
  observe({
    
    currentvar2 <- input$var2vis
    
    var1 <- input$var1vis
    facet1 <- input$facet1
    facet2 <- input$facet2
  
  updateSelectizeInput(session, "var2vis", choices = c(setdiff(variables$predictorVars, c(var1, facet1, facet2))), selected = currentvar2)
  
  })
  
  observe({
    
    currentfacet1 <- input$facet1
    
    var1 <- input$var1vis
    var2 <- input$var2vis
    facet2 <- input$facet2
    
  updateSelectizeInput(session, "facet1", choices = c(setdiff(variables$predictorVars, c(var1, var2, facet2))), selected = currentfacet1)
  
  })
  
  observe({
    
    currentfacet2 <- input$facet2
    
    var1 <- input$var1vis
    var2 <- input$var2vis
    facet1 <- input$facet1
  updateSelectizeInput(session, "facet2", choices = c(setdiff(variables$predictorVars, c(var1, var2, facet1))), selected = currentfacet2)
  
  })
  
  
})
