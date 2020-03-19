#
# Free-Clust: Shiny app for clustering data
# Author: Maciej Dobrzynski
#
# This is the UI logic for a Shiny web application.
#


library(shiny)
#library(shinyjs) #http://deanattali.com/shinyjs/
library(shinyBS)

shinyUI(fluidPage(
  #useShinyjs(),
  
  # Application title
  title = "FreeClust",
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(width = 3,
      # Load data
      fileInput(
        'fileDataLoad',
        actionLink("alDataFormat", 'Select file and click Load Data'),
        accept = c('text/csv', 'text/comma-separated-values,text/plain')
      ),
      
      actionButton("butDataLoad",  'Load Data'),
      
      actionButton("butDataGen1", 'Synthetic data'),
      bsTooltip("butDataGen1", 
                "Use classic iris dataset for testing.",
                placement = "top",
                trigger = "hover"),

      br(),
      br(),
      
      radioButtons(
        "rBflipRowCol",
        "Samples in:",
        choices = list(
          "rows" = "row",
          "columns" = "col"
        ),
        selected = "row"
      ),
      bsTooltip("rBflipRowCol", 
                "Layout of data in the input file.",
                placement = "top",
                trigger = "hover"),
      
      
      radioButtons(
        'rButDataNA',
        'Missing values represented by:',
        choices = list(
          'empty space' = '',
          'dash "-"' = '-',
          'NA' = 'NA'
        ),
        selected = ''
      ),
      radioButtons(
        'rButDataSep',
        'Column values separated by:',
        choices = list('comma ,' = ',',
                       'semicolon ;' = ';'),
        selected = ','
      ),
      radioButtons(
        'rButDataDec',
        'Decimal point:',
        choices = list('dot .' = '.',
                       'comma ,' = ','),
        selected = '.'
      ),
      
      #actionButton("butReset", "Reset file input"),
    ),
    
    mainPanel(width = 9,
      tabsetPanel(
        
        # Show a plot of the distribution
        tabPanel(
          'Histogram',
          dataHistUI('TabDataHist')
        ),
        
        # Hierarchical clustering (hclust)
        tabPanel(
          'Hierarchical',
          clustHierUI('TabClustHier')
        ),
        
        # Sparse hierarchical clustering (sparcl)
        tabPanel(
          'Sparse Hier.',
          clustHierSparUI('TabClustHierSpar')
        ),
        
        # tabPanel(
        #   'Bayesian',
        #   clustBayUI('TabClustBay')
        # ),
        
        # cluster validation
        tabPanel(
          'Validation',
          clustValidUI('TabClValid')
        )
        
      )
    )
  )))
