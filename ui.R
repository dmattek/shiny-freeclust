#
# Free-Clust: Shiny app for clustering data
# Author: Maciej Dobrzynski
#
# This is the UI logic for a Shiny web application.
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
                "Use classic iris dataset for testing.",
                placement = "top",
                trigger = "hover"),
      
      actionButton("butReset", "Reset file input"),
      # End of data load
      
      tags$hr(),
      
      # Data modification
      checkboxInput('chBdataNA20',
                    'Missing values to 0\'s',
                    FALSE),
      bsTooltip("chBdataNA20", 
                "Convert missing values to 0. Conversion is necessary for some algorithms, e.g. Bayesian.",
                placement = "top",
                trigger = "hover"),
      
      
      checkboxInput('chBdataScale',
                    'z-score',
                    FALSE),
      bsTooltip("chBdataScale", 
                "Rescale data with z-score. For every measurement (not sample!) subtract the mean and divide by SD.",
                placement = "top",
                trigger = "hover"),
      
      
      checkboxInput('chBdataLog',
                    'Log10(x)',
                    FALSE),
      bsTooltip("chBdataLog", 
                "Transform data with log10(x). Negative values and zeroes are changed to missing data points.",
                placement = "top",
                trigger = "hover"),

      checkboxInput('chBdataLog1',
                    'Log10(x+1)',
                    FALSE),
      bsTooltip("chBdataLog1", 
                "Transform data with log10(x+1). Good for x >= 0. Negative values and zeroes are changed to missing data points.",
                placement = "top",
                trigger = "hover"),

      bsAlert("alertAnchorNegPresent"),
      
      
      checkboxInput('chBdataWinsor2',
                    'Winsorise',
                    FALSE),
      bsTooltip("chBdataWinsor2", 
                "Pull-in data points that are farther than 3x robust SD (MAD). The procedure is similar to clipping.",
                placement = "top",
                trigger = "hover"),
      
      
      tags$hr(),
      
      # Show data min/max
      textOutput('dataMin'),
      textOutput('dataMax'),
      
      tags$hr(),
      
      # trim or clip data
      checkboxInput('chBdataTrim', 'Trim data'),
      bsTooltip("chBdataTrim", 
                "Remove data points outside of the range and set them to NA.",
                placement = "top",
                trigger = "hover"),
      
      uiOutput('resetable_input_trim'),
      uiOutput('uiButTrim'),
      
      checkboxInput('chBdataClip', 'Clip data'),
      bsTooltip("chBdataClip", 
                "Remove data points outside of the range and set them to range limits.",
                placement = "top",
                trigger = "hover"),
      
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
        # The package is not available on CRAN anymore,
        # install from the archive https://cran.r-project.org/src/contrib/Archive/bclust/
        # then uncomment here, in server.R and in global.R
        #
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
