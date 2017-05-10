



# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinyjs) #http://deanattali.com/shinyjs/


shinyUI(fluidPage(
  # Application title
  titlePanel("FreeClust"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      # Load data
      fileInput(
        'fileDataLoad',
        'Select data file and press "Load Data"',
        accept = c('text/csv', 'text/comma-separated-values,text/plain')
      ),
      helpPopup(
        title = 'Load Data',
        content = 'Accepts CSV text file with samples as columns, measurements (features) as rows. First row should contain samples\' names; first column should contain features\' names',
        placement = 'right',
        trigger = 'hover'
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
      actionButton('butDataGen1', 'Generate artificial dataset'),
      helpPopup(
        title = 'Generate artificial dataset',
        content = 'Generates random dataset for testing',
        placement = 'right',
        trigger = 'hover'
      ),
      actionButton("butReset", "Reset file input"),
      # End of data load
      
      tags$hr(),
      
      # Data modification
      fluidRow(column(
        6,
        checkboxInput('chBdataNA20',
                      'Convert missing values to 0\'s',
                      FALSE)
      ),
      column(
        6,
        helpPopup(
          title = 'Convert missing values to 0',
          content = 'Conversion affects clustering but is necessary for some algorithms, e.g. Bayesian.',
          placement = 'right',
          trigger = 'hover'
        )
      )),
      
      fluidRow(column(
        6,
        checkboxInput('chBdataScale',
                      'Rescale data',
                      FALSE)
      ),
      column(
        6,
        helpPopup(
          title = 'Rescale data',
          content = 'For every species: subtract the mean, divide by STD. This is equivalent to Z-score.',
          placement = 'right',
          trigger = 'hover'
        )
      )),
      
      
      fluidRow(column(
        6,
        checkboxInput('chBdataLog',
                      'Take log10 of data',
                      FALSE)
      ),
      column(
        6,
        helpPopup(
          title = 'log10 of data',
          content = 'Transform your data with log10. Missing fields or missing fields converted to 0 are omitted in this transformation.',
          placement = 'right',
          trigger = 'hover'
        )
      )),
      
      fluidRow(column(
        6,
        checkboxInput('chBdataWinsor2',
                      'Winsorise data',
                      FALSE)
      ),
      column(
        6,
        helpPopup(
          title = 'Winsorise data',
          content = 'Pull-in data points that are farther than 3x robust SD (MAD). The procedure is similar to trimming.',
          placement = 'right',
          trigger = 'hover'
        )
      )),
      
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
        'Data histogram',
        br(),
        p(
          'Overview of your data. Avoid long tails or strong assymetries in the histogram by converting your data to Z-scores,
          taking logarithm of data, or by removing the outliers.'
        ),
        br(),
        sliderInput(
          'slHistBinN',
          'Set the number of histogram bins',
          min = 1,
          max = 100,
          value = 10,
          step = 1
        ),
        plotOutput('plotHist', width = '100%')
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
        )
    )
    )
)))
