# ui.R - User interface for the data exploration and classifier construction tool

library(ggvis)
library(glmnet)
library(magrittr)

shinyUI(navbarPage(h3("Exploratory binary classifier construction"),
                   
        tabPanel(submitButton(text = h4("Upload and view data")),
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
        tabPanel(submitButton(text = h4("Classifier construction")),
                 fluidRow(
                   column(2,
                          selectizeInput("classifier", 
                                         h4("Select classifier"),
                                         choices = list("Logistic L1 regression" = "logit",
                                                        "Linear L1 regression" = "linear")),
                          renderUI("penalty")),
                   column(2,
                          h4("Select model varables"),
                          uiOutput("maineffects"),
                          uiOutput("interactions")
                          ),
                   column(1,
                          submitButton(text = "Fit model", icon = icon("beer"))                          ),
                   column(7,
                          uiOutput("vismargins")
                          )
                 ),
                 fluidRow(
                   column(5,
                          h3("Variable selection"),
                          hr(),
                          ggvisOutput("mainEffectsPlot"),
                          ggvisOutput("interactionplot")
                            ),
                   column(7,
                          h3("Boundary visualization"),
                   hr(),
                   h4("Visualization settings"))
                   )
                 ),
        tabPanel(submitButton(text = h4("Classifier performance")))
        ))
