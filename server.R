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
    
    tosummarize <- input$tosummarize
    
    cat(ifelse(is.null(tosummarize), "No variables chosen. Select variables to display summary statistics.", summary(dataset[match(tosummarize, names(dataset))])))
    
  })
  
  output$tosummarize <- renderUI({
    
    dataset <- inputData()
    
    vars <- names(dataset)
    
    selectizeInput("tosummarize", "Number of variables to display in summary: ", choices = vars, multiple = TRUE)
    
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
                              predictorVars_nofactors = NULL,
                              varsInModel = NULL,
                              oldVarsInModel = NULL)
  
  fittedmod <- reactiveValues(fit = NULL,
                              prediction = NULL,
                              error = NULL,
                              penalty = NULL,
                              optimal = NULL)

  # observe dataset, allVars, response, and predictors
  observe({
    
    dataset <- inputData()
    variables$allVars <- names(dataset)

    variables$responseVar <- input$response
    variables$predictorVars <- setdiff(variables$allVars, variables$responseVar)
    
    whichAreFactors <- apply(dataset[,match(variables$predictorVars, names(dataset))], 2, is.factor)
    
    variables$predictorVars_nofactors <- variables$predictorVars[!whichAreFactors]

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
    fittedmod$optimal <- fitmod$optimal    

    varsinmod <- isolate(variables$varsInModel)
    oldvarsinmod <- isolate(variables$oldVarsInModel)
    
    if(!identical(varsinmod, oldvarsinmod)){
      updateSliderInput(session, "penalty", value = fitmod$optimal, min = 0, max = 2*signif(max(fitmod$penalty),3), step = 0.001)
      variables$oldVarsInModel <- varsinmod
    }
    
  })
  
  observeEvent(input$setoptimal,{
    fitmod <- model()
    updateSliderInput(session, "penalty", value = fitmod$optimal, min = 0, max = 2*signif(max(fitmod$penalty),3), step = 0.001)
  })
  

  # main effects plot
  output$mainEffectsPlot <- renderPlot({    
    
    fitreactive()
    
    predictors <- isolate(variables$predictorVars)
    varsinmodel <- isolate(variables$varsInModel)
    responsevar <- isolate(variables$responseVar)
    error <- isolate(fittedmod$error)
    
    mainEffectPlot(allVariables = predictors,
                   varsInModel = varsinmodel,
                   response = responsevar,
                   data = inputData(),
                   error=error)
    
  })
  
  output$interactionplot <- renderPlot({
    
    fitreactive()
    
    varsinmodel <- isolate(variables$varsInModel)
    error <- isolate(fittedmod$error)
    
    interactionPlot(varsInModel = varsinmodel,
                   data = inputData(),
                   error=error)
    
  })
  
  

  
  output$printpenalty <- renderText({
    
    fittedmod <- model()
    lambda <- isolate(fittedmod$penalty)
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
    predic_vars <- isolate(variables$predictorVars)
    predic_vars_nofactors <- isolate(variables$predictorVars_nofactors)
    

    # generate selectizeInputs
    list(
      
      selectizeInput("var1vis", "X - Axis", choices = c(NULL, predic_vars_nofactors)),
      selectizeInput("var2vis", "Y - Axis", choices = c(NULL, predic_vars_nofactors)),
      selectizeInput("facet1", "X - Facet", choices = c(NULL, predic_vars)),
      selectizeInput("facet2", "Y - Facet", choices = c(NULL, predic_vars))
      
    )
    
  })
  
  # prevent overlapping selection of margins and facets
  observe({
    
    currentvar1 <- input$var1vis
    
    var2 <- input$var2vis
    facet1 <- input$facet1
    facet2 <- input$facet2
    
    updateSelectizeInput(session, "var1vis", choices = c(NULL, setdiff(variables$predictorVars_nofactors, c(var2, facet1, facet2))), selected = currentvar1)
    
  })
  
  observe({
    
    currentvar2 <- input$var2vis
    
    var1 <- input$var1vis
    facet1 <- input$facet1
    facet2 <- input$facet2
  
  updateSelectizeInput(session, "var2vis", choices = c(NULL, setdiff(variables$predictorVars_nofactors, c(var1, facet1, facet2))), selected = currentvar2)
  
  })
  
  observe({
    
    currentfacet1 <- input$facet1
    
    var1 <- input$var1vis
    var2 <- input$var2vis
    facet2 <- input$facet2
    
  updateSelectizeInput(session, "facet1", choices = c(NULL, setdiff(variables$predictorVars, c(var1, var2, facet2))), selected = currentfacet1)
  
  })
  
  observe({
    
    currentfacet2 <- input$facet2
    
    var1 <- input$var1vis
    var2 <- input$var2vis
    facet1 <- input$facet1
  updateSelectizeInput(session, "facet2", choices = c(NULL, setdiff(variables$predictorVars, c(var1, var2, facet1))), selected = currentfacet2)
  
  })
  
  
  # main plot
  mainplotreactive <- reactive({
    
    input$boundaryButton
    
  })
  
  output$boundaryplot <- renderPlot({
    
    mainplotreactive()
    
    xvar <- isolate(input$var1vis)
    yvar <- isolate(input$var2vis)
    facetx <- isolate(input$facet1)
    facety <- isolate(input$facet2)
    
    response <- isolate(variables$responseVar)
    
    predictions <- isolate(fittedmod$prediction)
    
    dat <- inputData()
    
    isolate(mainPlotFunction(xVar = xvar, yVar = yvar, facetX = facetx, facetY = facety, response = response, data = dat, predictions = predictions))
    
  })
  
  output$printmargins <- renderPrint(({
    
    c(input$var1vis, class(input$var1vis), 
      input$var2vis, class(input$var2vis), 
      input$facet1, class(input$facet1),
      input$facet2, class(input$facet2))
    
  }))
  
  output$cvplot <- renderPlot({
    plotCV(fittedmod$fit)
  })
  
  output$rocplot <- renderPlot({
    plotROC(variables$responseVar, fittedmod$prediction, inputData())
  })
  
})

  
