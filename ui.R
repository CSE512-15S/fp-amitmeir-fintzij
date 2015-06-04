# ui.R - User interface for the data exploration and classifier construction tool

library(ggvis)
library(glmnet)
library(magrittr)

shinyUI(navbarPage("Exploratory binary classifier construction",
                   
        tabPanel("Upload and view data",
             sidebarLayout(
               sidebarPanel(fileInput('dataset', 'Upload dataset with at least one binary response:',
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
                   column(3,
                          selectizeInput("classifier", 
                                         h4("Select classifier"),
                                         choices = list("Logistic L1 regression" = "logit",
                                                        "Linear L1 regression" = "linear"))),
                   column(3,
                          numericInput("lambda", label = h4("Penalty"), value = 0)
                          ),
                   column(6,
                          strong("Response: "), em(textOutput("printresponse", inline = T)),
                          br(),
                          strong("Predictors: "), em(textOutput("printpreds", inline = T))
                          )
                 ),
                 fluidRow(
                   column(6,
                          h3("Variable selection"),
                          hr(),
                          ggvisOutput("mainEffectsPlot"),
                          ggvisOutput("interactionplot"),
                          textOutput("printlambda")
                            ),
                   column(6,
                          h3("Boundary visualization"),
                   hr(),
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
