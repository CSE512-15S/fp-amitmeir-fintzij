# server.R performs the server-side calculations for ui.R

source("InteractionPlotFunction.R")
source("basicViz.R")
source("basicVizGGVIS.R")
library(glmnet)
library(ggvis)

shinyServer(function(input, output, session) { 
  # reactive expression for the dataset
  
  inputData <- reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can be found.
    inFile <- input$dataset

    if (is.null(inFile))

    return(iris)

    read.csv(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote)

  })
  
  
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
  
  output$variables <- renderUI({
    
    # get dataset
    inFile <- inputData()
    
    # extract variable names
    varnames <- names(inFile)
    
    # generate selectizeInputs
    list(
      selectizeInput("response", "Response Variable", choices = varnames),
      selectizeInput("predictors", "Predictor Variables", choices = varnames, multiple = TRUE)
      )
    
  })
  
  
  # selection of identical predictor and response 
  
  # REACTIVE VALUES :
  R <- reactiveValues(oldpredictors=0, oldresponse=0) 
  # oldpredictors:  the previous value of predictors when Ex occurs
  # oldresponse:  the previous value of response when Ey occurs
  # REACTIVE VALUES :
  XY <- reactiveValues(predictors=NULL,  response=NULL, count=0)
  # predictors: the effective value of predictors
  # response: the effective value of response
  # count:  count each time predictors=response ('always 0' = 'app successful')
  observe({
    if(!is.null(XY$predictors) && !is.null(XY$response)) XY$count <- isolate(XY$count) + any(XY$predictors==XY$response)
  })
  output$count <- renderText({ XY$count })
  ##
  #
  # the case of one of the predictors set to the same value as the reponse
  #
  observe({
    if(!is.null(input$response)){
      R$oldpredictors <- isolate(input$predictors)
    }
  })
  observe({
    if(!is.null(input$predictors)){
      if(all(input$predictors!=isolate(input$response))){
        R$oldpredictors <- input$predictors
        XY$predictors <- input$predictors; XY$response <- isolate(input$response)
      }
      else{ # we exchange the matching predictor and response 
        oldpredictors <- isolate(R$oldpredictors)
        updateSelectInput(session, "response", choices=names(input$dataset), selected=oldpredictors)
        XY$predictors <- input$predictors; XY$response <- oldpredictors 
      }
    }
  })
  ##
  #
  # 2) the case of the response being set to the same value as a predictor
  #
  observe({
    if(!is.null(input$predictors)){
      R$oldresponse <- isolate(input$response)
    }
  })
  observe({
    if(!is.null(input$response)){
      if(all(input$response!=isolate(input$predictors))){
        R$oldresponse <- input$response
        XY$predictors <- isolate(input$predictors); XY$response <- input$response
      }
      else{  #  exchange predictors and response 
        oldresponse <- isolate(R$oldresponse)
        updateSelectInput(session, "predictors", choices=names(input$dataset), selected=oldresponse)
        XY$predictors <- oldresponse; XY$response <- input$response
      }
    }
  })
  
  
  
  # selectizeInput for plot margins
  output$vismargins <- renderUI({
    
    # extract variable names
    predic_vars <- input$predictors
    
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
  # oldvar1vis:  the previous value of var1vis when Ex occurs
  # oldvar2vis:  the previous value of var2vis when Ey occurs
  # REACTIVE VALUES :
  UV <- reactiveValues(var1vis=NULL,  var2vis=NULL, count=0)
  # var1vis: the effective value of var1vis
  # var2vis: the effective value of var2vis
  # count:  count each time var1vis=var2vis ('always 0' = 'app successful')
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