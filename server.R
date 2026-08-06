#
# Free-Clust: Shiny app for clustering data
# Author: Maciej Dobrzynski
#
# This is the server logic for a Shiny web application.
#

library(shiny)
#library(shinyjs) #http://deanattali.com/shinyjs/
library(shinyBS) # for tooltips
library(shinycssloaders) # for loader animations

## Global parameters ----
# change to increase the limit of the upload file size
options(shiny.maxRequestSize = 100 * 1024 ^ 2)

# colour of loader spinner (shinycssloaders)
options(spinner.color="#00A8AA")

## SERVER ----

function(input, output, session) {
  #useShinyjs()
  
  # This is only set at session start
  # we use this as a way to determine which input was
  # clicked in the dataInBoth reactive
  counter <- reactiveValues(
    # The value of inDataGen1,2 actionButton is the number of times they were pressed
    dataGen1 = isolate(input$butDataGen1),
    dataLoad = isolate(input$butDataLoad)
  )
  
  # This button will reset the inFileDataLoad
  # observeEvent(input$butReset, {
  #   shinyjs::reset("fileDataLoad")  # reset is a shinyjs function
  # })
  
  ## Load data ----
  # load main data file; 
  # return a matrix with samples as rows, measurements/features as columns
  dataLoad <- eventReactive(input$butDataLoad, {
    myDebug("dataLoad\n")
    locFilePath = input$fileDataLoad$datapath
    
    counter$dataLoad <- input$butDataLoad - 1
    
    if (is.null(locFilePath) || locFilePath == '') {
      myDebug("dataLoad: null\n")

      createAlert(
        session,
        "alertAnchorDataLoad",
        "alertDataLoadNoFile",
        title = "No file to load",
        content = helpText.server[["alertDataLoadNoFile"]],
        append = FALSE,
        style = "danger"
      )

      return(NULL)
    }
    else {
      myDebug("dataLoad: read\n")

      closeAlert(session, "alertDataLoadNoFile")

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

      # Every remaining column should hold numeric measurements.
      # A single text column would turn the entire matrix into a character one
      # in as.matrix below, which only surfaces much later as an opaque
      # "'x' must be numeric" from the histogram or the distance calculation.
      locBadCols = names(locDT)[!vapply(locDT, is.numeric, logical(1))]

      if (length(locBadCols) > 0) {
        myDebug("dataLoad: non-numeric columns\n")

        locBadColsTxt = paste(head(locBadCols, 5), collapse = ", ")
        if (length(locBadCols) > 5)
          locBadColsTxt = paste0(locBadColsTxt,
                                 ", and ",
                                 length(locBadCols) - 5,
                                 " more")

        createAlert(
          session,
          "alertAnchorDataLoad",
          "alertDataLoadNotNumeric",
          title = "Cannot read the data",
          content = sprintf(helpText.server[["alertDataLoadNotNumeric"]],
                            locBadColsTxt),
          append = FALSE,
          style = "danger"
        )

        return(NULL)
      }

      closeAlert(session, "alertDataLoadNotNumeric")

      # Sample names have to be unique: the clustering itself copes with
      # repeated names, but building the heatmap's row annotation from them
      # fails outright with "duplicate 'row.names' are not allowed".
      # Keep every row and disambiguate the names instead of refusing the file.
      locDupIDs = unique(loc1stColVal[duplicated(loc1stColVal)])

      if (length(locDupIDs) > 0) {
        myDebug("dataLoad: duplicated sample names\n")

        locDupIDsTxt = paste(head(locDupIDs, 5), collapse = ", ")
        if (length(locDupIDs) > 5)
          locDupIDsTxt = paste0(locDupIDsTxt,
                                ", and ",
                                length(locDupIDs) - 5,
                                " more")

        createAlert(
          session,
          "alertAnchorDataLoad",
          "alertDataLoadDupIDs",
          title = "Duplicated sample names",
          content = sprintf(helpText.server[["alertDataLoadDupIDs"]],
                            locDupIDsTxt),
          append = FALSE,
          style = "warning"
        )

        loc1stColVal = make.unique(as.character(loc1stColVal))
      } else {
        closeAlert(session, "alertDataLoadDupIDs")
      }

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
    
    myDebug(
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
      myDebug("dataInBoth: inDataGen1\n")
      dm = myUserDataGenIris()
      # complaints about a previously loaded file do not apply to this data
      for (locAlertId in c("alertDataLoadNoFile",
                           "alertDataLoadNotNumeric",
                           "alertDataLoadDupIDs"))
        closeAlert(session, locAlertId)
      # no need to isolate updating the counter reactive values!
      counter$dataGen1 <- locInGen1
    } else if (locInDataLoad != isolate(counter$dataLoad)) {
      myDebug("dataInBoth: inDataLoad\n")
      dm = dataLoad()
      # no need to isolate updating the counter reactive values!
      counter$dataLoad <- locInDataLoad
    } else {
      myDebug("dataInBoth: else\n")
      dm = NULL
    }
    return(dm)
  })
  
  # return dt modified according to UI
  dataMod <- reactive({
    myDebug('dataMod\n')
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
  dataModProc = dataHist('TabDataHist', dataMod)

  ##### Hierarchical clustering: hclust
  clustHier('TabClustHier', dataModProc)

  ##### Sparse hierarchical clustering using sparcl
  clustHierSpar('TabClustHierSpar', dataModProc)

  ##### Bayesian clustering is retired, see retired/README.md

  ##### Hierarchical validation
  clustValid('TabClValid', dataModProc)
  
  # Pop-overs ----
  addPopover(session, 
             "alDataFormat",
             title = "Data format",
             content = helpText.server[["alDataFormat"]],
             trigger = "click")

}
