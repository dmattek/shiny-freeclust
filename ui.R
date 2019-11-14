



# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinyjs) #http://deanattali.com/shinyjs/
library(shinyBS)

shinyUI(fluidPage(
  useShinyjs(),
  
  # Application title
  title = "FreeClust",
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      # Load data
      fileInput(
        'fileDataLoad',
        actionLink("alDataFormat", 'Select file and click Load Data'),
        accept = c('text/csv', 'text/comma-separated-values,text/plain')
      ),
      radioButtons(
        'rButDataNA',
        'Missing values represented by:',
        choices = list(
          'NA' = 'NA',
          'Dash -' = '-',
          'Empty space' = ''
        ),
        selected = '-'
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
      actionButton("butDataLoad",  'Load Data'),
      
      actionButton("butDataGen1", 'Synthetic data'),
      bsTooltip("butDataGen1", 
                "Generates random dataset for testing",
                placement = "top",
                trigger = "hover"),
      
      actionButton("butReset", "Reset file input"),
      # End of data load
      
      tags$hr(),
      
      # Data modification
      checkboxInput('chBdataNA20',
                    'Convert missing values to 0\'s',
                    FALSE),
      bsTooltip("chBdataNA20", 
                "Conversion affects clustering but is necessary for some algorithms, e.g. Bayesian.",
                placement = "top",
                trigger = "hover"),
      
      
      checkboxInput('chBdataScale',
                    'Rescale data',
                    FALSE),
      bsTooltip("chBdataScale", 
                "For every measurement (not sample!) subtract the mean, divide by SD. This is equivalent to calculating a z-score.",
                placement = "top",
                trigger = "hover"),
      
      
      checkboxInput('chBdataLog',
                    'Take log10 of data',
                    FALSE),
      bsTooltip("chBdataLog", 
                "Transform data with log10. Negative values and zeroes are changed to missing data points.",
                placement = "top",
                trigger = "hover"),
      bsAlert("alertAnchorNegPresent"),
      
      
      checkboxInput('chBdataWinsor2',
                    'Winsorise data',
                    FALSE),
      bsTooltip("chBdataWinsor2", 
                "Pull-in data points that are farther than 3x robust SD (MAD). The procedure is similar to trimming.",
                placement = "top",
                trigger = "hover"),
      
      
      tags$hr(),
      
      # Show data min/max
      textOutput('dataMin'),
      textOutput('dataMax'),
      
      tags$hr(),
      
      # trim or clip data
      checkboxInput('chBdataTrim', 'Trim data?'),
      uiOutput('resetable_input_trim'),
      uiOutput('uiButTrim'),
      
      checkboxInput('chBdataClip', 'Clip data?'),
      uiOutput('resetable_input_clip'),
      uiOutput('uiButClip')
    ),
    # End of data modification
    
    
    
    mainPanel(
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
        
        # Bayesian (bclust)
        tabPanel(
          'Bayesian',
          clustBayUI('TabClustBay')
        ),
        
        # cluster validation
        tabPanel(
          'Validation',
          clustValidUI('TabClValid')
        )
        
      )
    )
  )))
