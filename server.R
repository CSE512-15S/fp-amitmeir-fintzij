# server.R performs the server-side calculations for ui.R

shinyServer(function(input, output, session) {
          
          # reactive expression for the dataset
          inputData <- reactive({
                    # input$file1 will be NULL initially. After the user selects
                    # and uploads a file, it will be a data frame with 'name',
                    # 'size', 'type', and 'datapath' columns. The 'datapath'
                    # column will contain the local filenames where the data can
                    # be found.
                    
                    inFile <- input$file1
                    
                    if (is.null(inFile))
                              return(iris)
                    
                    read.csv(inFile$datapath, header = input$header,
                             sep = input$sep, quote = input$quote)
                    
          })
          
          # dynamic variable names
          observe({
                    data<-inputData()
                    updateSelectizeInput(session, 'vars', choices = names(data))
                    
          }) # end observe
          

          # server function to render the data table          
          output$contents <- renderDataTable({
                    
                    inputData()
                    
          }) 
          
          # server function to render the data summary
          output$summary <- renderPrint({
                    dataset <- inputData()
                    summary(dataset)
          })
          
          
          
})
