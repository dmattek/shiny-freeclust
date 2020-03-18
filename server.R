#
# Free-Clust: Shiny app for clustering data
# Author: Maciej Dobrzynski
#
# This is the server logic for a Shiny web application.
#

library(shiny)
library(shinyjs) #http://deanattali.com/shinyjs/
library(shinyBS) # for tooltips
library(shinycssloaders) # for loader animations

## Global parameters ----
# change to increase the limit of the upload file size
options(shiny.maxRequestSize = 100 * 1024 ^ 2)

# colour of loader spinner (shinycssloaders)
options(spinner.color="#00A8AA")

## SERVER ----

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
    
    return(myUerDataGenIris())
  })
  
  ## Load data ----
  # load main data file; 
  # return a matrix with samples as rows, measurements/features as columns
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
      
      locDT = fread(
        locFilePath,
        na.strings = input$rButDataNA,
        sep = input$rButDataSep,
        dec = input$rButDataDec
      )
      
      loc1stCol = locDT[, 1]
      loc1stColName = colnames(loc1stCol)
      loc1stColVal  = loc1stCol[[loc1stColName]]
      locDT[, (loc1stColName) := NULL]

      locDM = as.matrix(locDT)
      rownames(locDM) = loc1stColVal
      
      return(locDM)      
    }
  })
  
  ## Prepare data ----
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
      dm = myUserDataGenIris()
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
  
  # return dt modified according to UI
  dataMod <- reactive({
    cat(file = stdout(), 'dataMod\n')
    loc.dm = dataInBoth()
    
    if (is.null(loc.dm))
      return(NULL)
    
    if(input$rBflipRowCol == "col") {
      # work with data matrix, where:
      # row - categories/features
      # columns - samples
      
      loc.dm = t(loc.dm)
    }
    
    return(loc.dm)
  })
  
  ## Modules ----
  
  ##### Histogram of dataset
  dataModProc = callModule(dataHist, 'TabDataHist', dataMod)
  
  ##### Hierarchical clustering: hclust
  callModule(clustHier, 'TabClustHier', dataModProc)
  
  ##### Sparse hierarchical clustering using sparcl
  callModule(clustHierSpar, 'TabClustHierSpar', dataModProc)
  
  ##### Bayesian clustering
  callModule(clustBay, 'TabClustBay', dataModProc)
  
  ##### Hierarchical validation
  callModule(clustValid, 'TabClValid', dataModProc)
  
  # Pop-overs ----
  addPopover(session, 
             "alDataFormat",
             title = "Data format",
             content = helpText.server[["alDataFormat"]],
             trigger = "click")
  
})
