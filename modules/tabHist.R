#
# Free-Clust: Shiny app for clustering data
# Author: Maciej Dobrzynski
#
# This module is a tab for plotting a historgram
# Use:
# in ui.R
# tabPanel(
#  'Hierarchical',
#  clustHierUI('TabClustHier'))
#
# in server.R
# callModule(clustHier, 'TabClustHier', dataMod)
# where dataMod is the output from a reactive function 
# that returns a dataset in wide format ready for clustering
#
# Operations are optionally but not exclusively applied to data in the following order:
# 1. rescaling (either of: z-score, log10(x), log10(x+1), winsorize)
# 2. removal of NAs
# 3. Trimming
# 4. Clipping

library(magrittr) # provides %>% operator
library(shinycssloaders) # provides a spinner
library(shinyBS) # adds alert messages
library(shinycssloaders) # for loader animations

helpText.dataHist = c(alLearnMore = paste0("<p>Avoid long tails or strong assymetries in the histogram ",
                                           "by converting your data to z-scores, taking the logarithm, ",
                                           "or by removing/clipping the outliers.</p>",
                                           "<p>Numbers in brackets indicate the order ",
                                           "of operations applied to the dataset.</p>"),
                      alLearnMoreRescale = paste0("<p>Recsale data using:",
                                                  "<li>z-score - for every measurement across all samples subtract the mean and divide by SD.</li>",
                                                  "<li>log10(x) - transform and convert data ∈(-inf, 0] to NAs.</li>",
                                                  "<li>log10(x+1) - transform and convert data ∈(-inf, -1] to NAs. Useful when data ∈[0, +inf).</li>",
                                                  "<li>winsorize - clip data points that are farther than 3x robust SD (MAD).</li>",
                                                  "</p>"),
                      chBdataTrim = "Remove points outside of the range indicated by red vertical lines in the histogram, and set them to NA. ",
                      chBdataClip = "Remove points outside of the range indicated by blue vertical lines in the histogram, and set them to range limits.",
                      chBdataNA20 = "Convert missing values to 0. Conversion is necessary for some algorithms, e.g. Bayesian.",
                      alertNegPresent0 = "Data points smaller than or equal to 0 are present. Before applying log10(x), such points will be transformed to NAs.",
                      alertNegPresent1 = "Data points smaller than or equal to -1 are present. Before applying log10(x+1), such points will be transformed to NAs.")

# UI ----
dataHistUI <- function(id, label = "Histogram") {
  ns <- NS(id)
  
  tagList(
    h4('Data overview'),
    p(actionLink(ns("alLearnMore"), "Learn more")),
    
    br(),
    
    fluidRow(
      column(3, 
             sliderInput(
               ns('slHistBinN'),
               'Number of histogram bins',
               min = 1,
               max = 100,
               value = 10,
               step = 1
             )
      ),

      column(3,
             # rescale data
             selectInput(
               ns("selRescale"),
               p(actionLink(ns("alLearnMoreRescale"), "[1] Rescale data")),
               choices = c(
                 "no rescaling" = "noresc",
                 "z-score" = "zscore",
                 "log10(x)" = "log10x",
                 "log10(x+1)" = "log10xp1",
                 "winsorize" = "win"), 
               selected = "noresc",
               width = "75%"
             )
      ),
      
      column(3,
             checkboxInput(ns("chBdataNA20"),
                           "[2] Convert missing values to 0\'s",
                           FALSE),
             bsTooltip(ns("chBdataNA20"), 
                       helpText.dataHist[["chBdataNA20"]],
                       placement = "top",
                       trigger = "hover"),
             bsAlert("alertAnchorNegPresent")
      ),
      
      
      column(3, 
             # Show data min/max
             tags$b("Summary"),
             textOutput(ns('dataMin')),
             textOutput(ns('dataMax')),
             textOutput(ns('dataNA'))
      ),
    ),
    
    plotOutput(ns('plotHist'), 
               width = '100%'),
    tags$hr(),
    
    fluidRow(
      
      column(3,
             # trim data
             checkboxInput(ns('chBdataTrim'), '[3] Trim data'),
             bsTooltip(ns("chBdataTrim"), 
                       helpText.dataHist[["chBdataTrim"]],
                       placement = "top",
                       trigger = "hover"),
             conditionalPanel(
               condition = "input.chBdataTrim",
               ns = ns,
               
               numericInput(
                 ns('inDataTrimMin'),
                 'Discard data below:',
                 value = 0,
                 width = 200,
                 step = 100
               ),
               numericInput(
                 ns('inDataTrimMax'),
                 'Discard data above:',
                 value = 1e6,
                 width = 200,
                 step = 100
               ),
               actionButton(ns('butDataTrimReset'), 
                            'Reset trimming')
             )    
      ),
      
      column(3,
             # clip data
             checkboxInput(ns('chBdataClip'), '[4] Clip data'),
             bsTooltip(ns("chBdataClip"), 
                       helpText.dataHist[["chBdataClip"]],
                       placement = "top",
                       trigger = "hover"),
             
             conditionalPanel(
               condition = "input.chBdataClip",
               ns = ns,
               
               numericInput(
                 ns('inDataClipMin'),
                 'Clip data below:',
                 value = 0,
                 width = 200,
                 step = 100
               ),
               numericInput(
                 ns('inDataClipMax'),
                 'Clip data above:',
                 value = 1e6,
                 width = 200,
                 step = 100
               ),
               actionButton(ns('butDataClipReset'), 
                            'Reset clipping')
             )
      )
    )
    
  )
}

# SERVER ----
dataHist <- function(input, output, session, inDataMod) {
  
  ns <- session$ns
  
  # Dynamic UI ----
  output$dataMin <- renderText({
    cat(file = stdout(), 'tabHist:dataMin\n')
    
    loc.extr = calcDataMin()
    
    validate(
      need(!is.null(loc.extr), "Min/max unavailable. Load data first!")
    )
    
    if (!is.null(loc.extr)) {
      paste0('Min = ',
             formatC(
               loc.extr,
               format = "g",
               big.mark = '\'',
               decimal.mark = '.'
             ))
    }
  })
  
  output$dataMax <- renderText({
    cat(file = stdout(), 'tabHist:dataMax\n')
    
    loc.extr = calcDataMax()
    
    if (!is.null(loc.extr)) {
      paste0('Max = ',
             formatC(
               loc.extr,
               format = "g",
               big.mark = '\'',
               decimal.mark = '.'
             ))
    }
  })

  output$dataNA <- renderText({
    cat(file = stdout(), 'tabHist:dataNA\n')
    
    locX = calcDataNA()
    
    if (!is.null(locX)) {
      paste0('#NA = ',
             prettyNum(locX))
    }
  })
  
    
  # Set min/max numeric inputs for trimming and clipping to 
  # min/max values in the data
  observe({
    cat(file = stdout(), 'tabHist:observe:updateNumericInput\n')
    
    locMin = calcDataMin()
    locMax = calcDataMax()
    
    if(!is.null(locMin) & (!is.null(locMax))) {
      
      updateNumericInput(session, "inDataTrimMin", 
                         value = locMin,
                         min = locMin,
                         step = (locMax - locMin) / 100)
      updateNumericInput(session, "inDataTrimMax", 
                         value = locMax,
                         max = locMax,
                         step = (locMax - locMin) / 100)
      
      updateNumericInput(session, "inDataClipMin", 
                         value = locMin,
                         min = locMin,
                         step = (locMax - locMin) / 100)
      updateNumericInput(session, "inDataClipMax", 
                         value = locMax,
                         max = locMax,
                         step = (locMax - locMin) / 100)
    }
  })
  
  # Reset min/max input fields for trimming
  observeEvent(input$butDataTrimReset, {
    cat(file = stdout(), 'tabHist:observeEvent:updateNumericInput\n')
    
    locMin = calcDataMin()
    locMax = calcDataMax()
    
    if(!is.null(locMin) & (!is.null(locMax))) {
      
      updateNumericInput(session, "inDataTrimMin", 
                         value = locMin,
                         min = locMin,
                         step = (locMax - locMin) / 100)
      updateNumericInput(session, "inDataTrimMax", 
                         value = locMax,
                         max = locMax,
                         step = (locMax - locMin) / 100)
    }
    
  })
  
  # Reset min/max input fields for clipping
  observeEvent(input$butDataClipReset, {
    cat(file = stdout(), 'tabHist:observeEvent:updateNumericInput\n')
    
    locMin = calcDataMin()
    locMax = calcDataMax()
    
    if(!is.null(locMin) & (!is.null(locMax))) {
      
      updateNumericInput(session, "inDataClipMin", 
                         value = locMin,
                         min = locMin,
                         step = (locMax - locMin) / 100)
      updateNumericInput(session, "inDataClipMax", 
                         value = locMax,
                         max = locMax,
                         step = (locMax - locMin) / 100)
    }
  })
  
  
  # Data processing ----
  
  # Calculate min/max of data
  calcDataMin = reactive({
    cat(file = stdout(), 'tabHist:calcDataMin\n')
    
    locDM = rescaledData()
    
    if (!is.null(locDM)) {
      locX = myMin(locDM, sig = SIGNIFDIGITSROUND, na.rm = T)
    } else {
      return(NULL)
    }
  })
  
  calcDataMax = reactive({
    cat(file = stdout(), 'tabHist:calcDataMax\n')
    
    locDM = rescaledData()
    
    if (!is.null(locDM)) {
      locX = myMax(locDM, sig = SIGNIFDIGITSROUND, na.rm = T)
    } else {
      return(NULL)
    }
  })
  
  
  calcDataNA = reactive({
    cat(file = stdout(), 'tabHist:calcDataNA\n')
    
    locDM = rescaledData()
    
    if (!is.null(locDM)) {
      locX = sum(is.na(locDM))
    } else {
      return(NULL)
    }
  })
  
  # apply rescaling methods
  rescaledData = reactive({
    cat(file = stdout(), 'tabHist:rescaledData\n')
    
    locDM = inDataMod()
    
    if (is.null(locDM)) {
      return(NULL)
    }
    
    # rescale data column-wise (along features)
    if (input$selRescale == "zscore") {
      locDM = scale(locDM, 
                    center = TRUE,  
                    scale = TRUE)
    }
    
    # take log10 of data
    if (input$selRescale == "log10x") {
      if (sum(locDM <= 0, na.rm = T) > 0) {
        # Transform data points smaller or equal to zero into NAs
        locDM[locDM <= 0] <- NA
        
        createAlert(session, "alertAnchorNegPresent", "alertNegPresent0", title = "Warning",
                    content = helpText.dataHist[["alertNegPresent0"]], 
                    append = FALSE,
                    style = "warning")
        
      } else {
        closeAlert(session, "alertNegPresent0")
      }
      
      locDM = log10(locDM)
    } else {
      closeAlert(session, "alertNegPresent0")
    }
    
    # take log10(1+x) of data
    if (input$selRescale == "log10xp1") {
      if (sum(locDM <= -1, na.rm = T) > 0) {
        # Transform data points smaller or equal to -1 into NAs
        locDM[locDM <= -1] <- NA
        
        createAlert(session, "alertAnchorNegPresent", "alertNegPresent1", title = "Warning",
                    content = helpText.dataHist[["alertNegPresent1"]], 
                    append = FALSE,
                    style = "warning")
        
      } else {
        closeAlert(session, "alertNegPresent1")
      }
      
      locDM = log10(locDM + 1)
    } else {
      closeAlert(session, "alertNegPresent1")
    }
    
    # winsorize
    if (input$selRescale == "win")
      locDM = myWinsor2(locDM)
    
    # convert missing values in the input data to 0's
    if (input$chBdataNA20)
      locDM[is.na(locDM)] <- 0
    
    return(locDM)
  })
  
  # apply data filters from UI (clipping, trimming)
  # return it to other modules
  dmReturn = reactive({ 
    cat(file = stdout(), 'tabHist:dmReturn\n')
    
    locDM = rescaledData()
    
    if (is.null(locDM)) {
      return(NULL)
    }
    
    # Data trimming
    # Data points outside of the range are set to NA.
    if (input$chBdataTrim) {
      locDM[locDM < as.numeric(input$inDataTrimMin)] = NA
      locDM[locDM > as.numeric(input$inDataTrimMax)] = NA
    }
    
    
    # Data clipping
    # Data points outside of the range are set to range limits.
    if (input$chBdataClip) {
      locDM[locDM < as.numeric(input$inDataClipMin)] = input$inDataClipMin
      locDM[locDM > as.numeric(input$inDataClipMax)] = input$inDataClipMax
    }
    
    return(locDM)
  })
  
  # Histogram ----
  
  # Read the number of bins from the slider with a delay
  giveMeNbins = reactive({
    cat(file = stdout(), 'tabHist:giveMeNbins\n')
    
    locNbins = input$slHistBinN
    
    return(locNbins)
  }) %>% debounce(millis = MILLIS)
  
  output$plotHist <- renderPlot({
    cat(file = stdout(), 'tabHist:plotHist\n')
    
    locDM = rescaledData()
    
    validate(
      need(!is.null(locDM), "Nothing to plot. Load data first!")
    )
    
    if (is.null(locDM))
      return(NULL)
    
    # generate bins based on input$bins from ui.R
    locVecBins = seq(min(locDM, na.rm = T), 
                     max(locDM, na.rm = T), 
                     length.out = giveMeNbins() + 1)
    
    # Draw a histogram with vertical lines corresponding to:
    # trimming - red dashed
    # clipping - blue dotted
    hist(locDM, 
         breaks = locVecBins, 
         freq = FALSE,
         main = 'Histogram of data',
         xlab = 'Values',
         col = 'darkgray', 
         border = 'white')
    
    abline(v = input$inDataTrimMin,
           col = 2,
           lty = 2)
    abline(v = input$inDataTrimMax,
           col = 2,
           lty = 2)
    
    abline(v = input$inDataClipMin,
           col = 4,
           lty = 3)
    abline(v = input$inDataClipMax,
           col = 4,
           lty = 3)
  })
  
  # Pop-overs ----
  addPopover(session, 
             ns("alLearnMore"),
             title = "Data histogram",
             content = helpText.dataHist[["alLearnMore"]],
             trigger = "click")
  
  addPopover(session, 
             ns("alLearnMoreRescale"),
             title = "Data histogram",
             content = helpText.dataHist[["alLearnMoreRescale"]],
             trigger = "click")
  
  return(dmReturn)
}