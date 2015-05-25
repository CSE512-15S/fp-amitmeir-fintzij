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
                     h3("Classifier parameters"),
                     uiOutput("variables"),
                     hr(),
                     h3("Classifier visualization"),
                     selectizeInput(
                       "var1vis",
                       "Select first margin",
                       ""
                     ),
                     selectizeInput(
                       "var2vis",
                       "Select second margin",
                       ""),
                     selectizeInput(
                       "facetvar",
                       "Select a facetting variable",
                       "")
                     ),
                   mainPanel(
                     )
                   )),
        tabPanel("Classifier evaluation")   
        ))
