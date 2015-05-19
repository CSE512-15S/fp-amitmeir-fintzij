# ui.R - User interface for the data exploration and classifier construction tool

library(ggvis)
library(magrittr)

shinyUI(navbarPage("Exploratory binary classifier construction",
                   
         tabPanel("Upload and view data",
                  sidebarLayout(
                            sidebarPanel(fileInput('file1', 'Choose file to upload',
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
         tabPanel("Explore data"),
         tabPanel("Classifier evaluation")          

          ))
