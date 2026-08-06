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
  
  # This button will reset the inFileDataLoad
  # observeEvent(input$butReset, {
  #   shinyjs::reset("fileDataLoad")  # reset is a shinyjs function
  # })

  ## Load data ----
  # Read the file named in the file input and return a matrix with samples as
  # rows and measurements/features as columns, or NULL if it cannot be used,
  # having raised an alert saying why.
  #
  # A plain function rather than a reactive: the observer below decides when a
  # load happens, so this only has to perform one.
  loadDataFile <- function() {
    myDebug("loadDataFile\n")
    locFilePath = input$fileDataLoad$datapath

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

      # Record how the file was read, so a download can say so later
      locDM = mySetProvenance(locDM, c(
        paste0("Data source: ", input$fileDataLoad$name),
        paste0("Column separator: ", input$rButDataSep),
        paste0("Decimal point: ", input$rButDataDec),
        paste0("Missing values marked by: ",
               ifelse(input$rButDataNA == "", "empty field", input$rButDataNA))
      ))

      return(locDM)
    }
  }

  ## Prepare data ----

  # The dataset in play, whichever of the two sources produced it last.
  # Both write here, so "the most recent click wins" needs no bookkeeping:
  # each source only has to say what it loaded, and when.
  dataIn <- reactiveVal(NULL)

  observeEvent(input$butDataGen1, {
    myDebug("dataIn: synthetic\n")

    # complaints about a previously loaded file do not apply to this data
    for (locAlertId in c("alertDataLoadNoFile",
                         "alertDataLoadNotNumeric",
                         "alertDataLoadDupIDs"))
      closeAlert(session, locAlertId)

    dataIn(mySetProvenance(myUserDataGenIris(),
                           "Data source: synthetic data (iris)"))
  })

  observeEvent(input$butDataLoad, {
    myDebug("dataIn: file\n")

    # NULL when the file could not be used; loadDataFile has said why
    dataIn(loadDataFile())
  })
  
  # return dt modified according to UI
  dataMod <- reactive({
    myDebug('dataMod\n')

    locDM = dataIn()

    if (is.null(locDM))
      return(NULL)

    # read the record before transposing, t() does not carry it over
    locProv = myGetProvenance(locDM)

    if (input$rBflipRowCol == "col") {
      # work with data matrix, where:
      # row - categories/features
      # columns - samples

      locDM = t(locDM)
    }

    return(mySetProvenance(locDM, c(
      locProv,
      paste0("Samples read from: ",
             ifelse(input$rBflipRowCol == "col", "columns", "rows"))
    )))
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
