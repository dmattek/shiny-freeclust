

# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinyjs) #http://deanattali.com/shinyjs/
library(shinyBS) # for tooltips
library(shinycssloaders) # for loader animations

# colour of loader spinner (shinycssloaders)
options(spinner.color="#00A8AA")

shinyServer(function(input, output, session) {
  useShinyjs()
  
  # This is only set at session start
  # we use this as a way to determine which input was
  # clicked in the dataInBoth reactive
  counter <- reactiveValues(
    # The value of inDataGen1,2 actionButton is the number of times they were pressed
    dataGen1 = isolate(input$butDataGen1),
    dataLoad = isolate(input$butDataLoad)
  )
  
  # This button will reset the inFileDataLoad
  observeEvent(input$butReset, {
    reset("fileDataLoad")  # reset is a shinyjs function
  })
  
  # generate random dataset 1
  dataGen1 <- eventReactive(input$butDataGen1, {
    cat("dataGen1\n")
    
    return(userDataGen())
  })
  
  # load main data file
  dataLoad <- eventReactive(input$butDataLoad, {
    cat("dataLoad\n")
    locFilePath = input$fileDataLoad$datapath
    
    counter$dataLoad <- input$butDataLoad - 1
    
    if (is.null(locFilePath) || locFilePath == '') {
      cat("dataLoad: null\n")
      return(NULL)
    }
    else {
      cat("dataLoad: read\n")
      
      loc.df = read.csv(
        locFilePath,
        na.strings = input$rButDataNA,
        sep = input$rButDataSep,
        dec = input$rButDataDec
      )
      
      row.names(loc.df) = loc.df[, 1]
      loc.df[, 1] = NULL
      # work with data matrix, where:
      # columns - categories
      # rows - samples
      return(as.matrix(t(loc.df)))
    }
  })
  
  dataInBoth <- reactive({
    # Without direct references to inDataGen1,2 and inFileLoad, inDataGen2
    #    does not trigger running this reactive once inDataGen1 is used.
    # This is one of the more nuanced areas of reactive programming in shiny
    #    due to the if else logic, it isn't fetched once inDataGen1 is available
    # The morale is use direct retrieval of inputs to guarantee they are available
    #    for if else logic checks!
    
    locInGen1 = input$butDataGen1
    locInDataLoad = input$butDataLoad
    
    cat(
      "dataInBoth\ninGen1: ",
      locInGen1,
      "   prev=",
      isolate(counter$dataGen1),
      "\ninDataNuc: ",
      locInDataLoad,
      "   prev=",
      isolate(counter$dataLoad),
      "\n"
    )
    
    # isolate the checks of counter reactiveValues
    # as we set the values in this same reactive
    if (locInGen1 != isolate(counter$dataGen1)) {
      cat("dataInBoth: inDataGen1\n")
      dm = userDataGen()
      # no need to isolate updating the counter reactive values!
      counter$dataGen1 <- locInGen1
    } else if (locInDataLoad != isolate(counter$dataLoad)) {
      cat("dataInBoth: inDataLoad\n")
      dm = dataLoad()
      # no need to isolate updating the counter reactive values!
      counter$dataLoad <- locInDataLoad
    } else {
      cat("dataInBoth: else\n")
      dm = NULL
    }
    return(dm)
  })
  
  # return dt with an added column with unique track object label
  dataMod <- reactive({
    cat(file = stderr(), 'dataMod\n')
    loc.dm = dataInBoth()
    
    if (is.null(loc.dm))
      return(NULL)
    
    if (input$chBdataScale)
      loc.dm = scale(loc.dm, center = TRUE,  scale = TRUE)
    
    # take log10 of data
    if (input$chBdataLog)
      loc.dm = log10(loc.dm)
    
    # winsorize
    if (input$chBdataWinsor2)
      loc.dm = winsor2(loc.dm)
    
    # convert missing values in the input data to 0's
    if (input$chBdataNA20)
      loc.dm[is.na(loc.dm)] <- 0
    
    
    # Data trimming
    # data points below a threshold are set to NA
    # this isn't affected by conversion to 0's above
    if (input$chBdataTrim) {
      loc.dm[loc.dm < as.numeric(input$inDataTrimMin) & loc.dm != 0] <- NA
      
      # data points above a threshold are set to NA
      # this isn't affected by conversion to 0's above
      loc.dm[loc.dm > as.numeric(input$inDataTrimMax)] <- NA
    }
    
    
    # Data clipping
    if (input$chBdataClip) {
      loc.dm[loc.dm < as.numeric(input$inDataClipMin) &
               loc.dm != 0] <- input$inDataClipMin
      loc.dm[loc.dm > as.numeric(input$inDataClipMax)] <-
        input$inDataClipMax
    }
    
    return(loc.dm)
  })
  
  #####
  ## Dynamic UI in the side panel
  output$dataMin <- renderText({
    cat(file = stderr(), 'dataMin \n')
    
    loc.dm = dataMod()
    
    if (is.null(loc.dm)) {
      paste('Min/max unavailable - data not loaded')
    }
    else {
      loc.extr = min(loc.dm, na.rm = TRUE)
      paste('Min = ',
            formatC(
              loc.extr,
              format = "g",
              big.mark = '\'',
              decimal.mark = '.'
            ),
            sep = '')
    }
  })
  
  output$dataMax <- renderText({
    cat(file = stderr(), 'dataMax \n')
    loc.dm = dataMod()
    
    if (is.null(loc.dm)) {
      return(NULL)
    }
    else {
      loc.extr = max(loc.dm, na.rm = TRUE)
      paste('Max = ',
            formatC(
              loc.extr,
              format = "g",
              big.mark = '\'',
              decimal.mark = '.'
            ),
            sep = '')
    }
  })
  
  # dynamic UI for trimming data
  output$resetable_input_trim <- renderUI({
    cat(file = stderr(), 'resetable_input_trim \n')
    
    if (input$chBdataTrim) {
      times <- input$butDataTrimReset
      
      div(
        id = letters[(times %% length(letters)) + 1],
        numericInput(
          'inDataTrimMin',
          'Discard data below:',
          value = 0,
          width = 200,
          step = 100
        ),
        numericInput(
          'inDataTrimMax',
          'Discard data above:',
          value = 1e6,
          width = 200,
          step = 100
        )
      )
    } else
      return(NULL)
  })
  
  output$uiButTrim <- renderUI({
    if (input$chBdataTrim) {
      actionButton('butDataTrimReset', 'Reset data trimming')
    } else
      return(NULL)
  })
  
  output$resetable_input_clip <- renderUI({
    cat(file = stderr(), 'resetable_input_clip \n')
    
    if (input$chBdataClip) {
      times <- input$butDataClipReset
      div(
        id = letters[(times %% length(letters)) + 1],
        numericInput(
          'inDataClipMin',
          'Clip data below threshold:',
          value = 0,
          width = 200,
          step = 100
        ),
        numericInput(
          'inDataClipMax',
          'Clip data above threshold:',
          value = 1e6,
          width = 200,
          step = 100
        )
      )
    } else
      return(NULL)
  })
  
  output$uiButClip <- renderUI({
    if (input$chBdataClip) {
      actionButton('butDataClipReset', 'Reset data clipping')
    } else
      return(NULL)
  })
  
  
  
  #####
  ## Histogram of dataset
  output$plotHist <- renderPlot({
    cat(file = stderr(), 'plotHist \n')
    
    loc.dm = dataMod()
    #cat(loc.dm)
    
    if (is.null(loc.dm))
      return(NULL)
    
    plot(
      hist(loc.dm, breaks = input$slHistBinN, freq = TRUE),
      main = 'Histogram of data',
      xlab = 'Values'
    )
  })
  
  ##### Hierarchical clustering: hclust
  callModule(clustHier, 'TabClustHier', dataMod)
  
  
  ##### Sparse hierarchical clustering using sparcl
  callModule(clustHierSpar, 'TabClustHierSpar', dataMod)
  
  ##### Bayesian clustering
  callModule(clustBay, 'TabClustBay', dataMod)
  
})
