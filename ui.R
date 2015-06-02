# ui.R - User interface for the data exploration and classifier construction tool

library(ggvis)
library(glmnet)
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
                            tags$hr(),
                            uiOutput("response")
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
                 fluidRow(
                   column(6,
                          h3("Variable selection")),
                   column(6,
                          h3("Boundary visualization"))
                   ),
                 hr(),
                 fluidRow(
                   column(4,
                          offset = 1,
                          h4("Select classifier"),
                          selectizeInput("classifier", 
                                         label = NULL, 
                                         choices = list("Logistic L1 regression" = "logit",
                                                        "Linear L1 regression" = "linear")),
                          textOutput("printresponse"),
                          uiOutput("tuning_params")
                          ),
                   column(4,
                          offset=2,
                          h4("Visualization settings"))
                   )
                 ),
#         tabPanel("Classifier construction",
#                  sidebarLayout(
#                    sidebarPanel(
#                      h3("Select classifier"),
#                      selectizeInput("classifier", 
#                                     "", 
#                                     choices = list("Logistic regression" = "logit",
#                                                 "Penalized logistic regression" = "pen_logit")),
# #                      hr(),
# #                      
# #                      h3("Classifier parameters"),
# #                      uiOutput("variables"),
#                      uiOutput("tuning_params"),
#                      
#                      hr(),
#                      
#                      h3("Visualization settings"),
#                      uiOutput("vismargins")
#                      ),
#                    mainPanel(
#                      )
#                    )),
        tabPanel("Classifier performance")
        ))
