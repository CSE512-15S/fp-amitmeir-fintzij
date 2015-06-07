# ui.R - User interface for the data exploration and classifier construction tool

library(ggplot2)
library(glmnet)
library(magrittr)

shinyUI(navbarPage("Exploratory binary classifier construction",
                   # upload panel
                   tabPanel(actionButton("uploadButton" , label = h5("Upload and view data")),
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
                   
                   # construction panel
                   tabPanel(actionButton("constructionButton" , label = h5("Classifier construction")),
                            fluidRow(
                              column(2,
                                     h3("Select model varables"),
                                     hr(),
                                     selectizeInput("classifier", 
                                                    h4("Select classifier"),
                                                    choices = list("Penalized logistic regression" = "logit",
                                                                   "Penalized linear regression" = "linear")),
                                     sliderInput("penalty", "Set L1 penalty parameter", min = 0, max = 0, value = 0,sep="", round = -3,step = 0.001),
                                     actionButton("setoptimal", "Reset to optimal penalty"),
                                     br(), br(), br(),
                                     textOutput("printpreds")
                                     ),
                              column(3,
                                     actionButton("fitButton", label = h3("Fit model")),
                                     br(), br(), br(),
                                     uiOutput("maineffects"),
                                     uiOutput("interactions")
                              ),
                              
                              column(2,
                                     h3("Boundary visualization"),
                                     hr(),
                                     uiOutput("vismargins")
                              ),
                              column(4,
                                     actionButton("boundaryButton", label = h3("Plot boundary"))
                                     )
                            ),
                            fluidRow(
                              column(5,
                                     hr(),
                                     h4("Correlation between main effects and model errors"),
                                     plotOutput("mainEffectsPlot", width = "100%", height = "300px"),
                                     h4("Correlation between interactions and model errors"),
                                     plotOutput("interactionplot")
                              ),
                              column(7,
                                     hr(),
                                     textOutput("printmargins"),
                                     plotOutput("boundaryplot")
                                     )
                            )
                   ),
                   tabPanel(actionButton("performanceButton", label = h5("Classifier performance")))
))
