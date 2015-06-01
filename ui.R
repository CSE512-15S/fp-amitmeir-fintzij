# ui.R - User interface for the data exploration and classifier construction tool

library(ggvis)
library(magrittr)

shinyUI(navbarPage("Exploratory binary classifier construction",
                   
        tabPanel("Upload and view data",
             sidebarLayout(
               sidebarPanel(fileInput('dataset', 'Choose file to upload',
                                      accept = c(
                                        'text/csv',
                                        'text/comma-separated-values',
                                        'text/tab-separated-values',
                                        'text/plain',
                                        '.csv',
                                        '.tsv'
                                        )
                                      ),
                            tags$hr(),
                            checkboxInput('header', 'Header', TRUE),
                            radioButtons('sep', 'Separator',
                                         c(Comma=',',
                                           Semicolon=';',
                                           Tab='\t'),
                                         ','),
                            radioButtons('quote', 'Quote',
                                         c(None='',
                                           'Double Quote'='"',
                                           'Single Quote'="'"),
                                         '"'),
                            tags$hr()
                                      ),
                            
                            mainPanel(
                              h3("Data summary"),
                              verbatimTextOutput("summary"),
                              hr(),
                              h3("Data"),
                              dataTableOutput('contents')
                              )
               )
             ),
        tabPanel("Classifier construction",
                 sidebarLayout(
                   sidebarPanel(
                     h3("Select classifier"),
                     selectizeInput("classifier", 
                                    "", 
                                    choices = list("Logistic regression" = "logit",
                                                "Penalized logistic regression" = "pen_logit",
                                                "Support vector machine" = "svm"))
                     hr(),
                     
                     h3("Classifier parameters"),
                     uiOutput("variables"),
#                      uiOutput("tuning_params"),
                     
                     hr(),
                     
                     h3("Visualization settings"),
                     uiOutput("vismargins")
                     ),
                   mainPanel(
                     )
                   )),
        tabPanel("Classifier evaluation")   
        ))
