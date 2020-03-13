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


library(magrittr) # provides %>% operator
library(shinycssloaders) # provides a spinner
library(shinyBS) # adds alert messages
require(shinycssloaders) # for loader animations

helpText.dataHist = c(alLearnMore = paste0("<p>Avoid long tails or strong assymetries in the histogram ",
                                           "by converting your data to Z-scores, taking logarithm of data, ",
                                           "or by removing the outliers.</p>"),
                      alLearnMoreRescale = paste0("<p>Recsale data using:",
                                                  "<li>z-score - for every measurement across all samples subtract the mean and divide by SD.</li>",
                                                  "<li>log10(x) - transform and convert data ∈(-inf, 0] to NAs.</li>",
                                                  "<li>log10(x+1) - transform and convert data ∈(-inf, -1] to NAs. Useful when data ∈[0, +inf).</li>",
                                                  "<li>winsorize - clip data points that are farther than 3x robust SD (MAD).</li>",
                                                  "</p>"),
                      chBdataTrim = "Remove data points outside of the range and set them to NA.",
                      chBdataClip = "Remove data points outside of the range and set them to range limits.",
                      chBdataNA20 = "Convert missing values to 0. Conversion is necessary for some algorithms, e.g. Bayesian."
)
# UI ----
dataHistUI <- function(id, label = "Histogram") {
  ns <- NS(id)
  
  tagList(
    h4('Data histogram'),
    p("Data overview. ",
      actionLink(ns("alLearnMore"), "Learn more")
    ),
    
    br(),
    
    fluidRow(
      column(4, 
             sliderInput(
               ns('slHistBinN'),
               'Number of histogram bins',
               min = 1,
               max = 100,
               value = 10,
               step = 1
             )
      ),
      
      column(4,
             
             checkboxInput(ns('chBdataNA20'),
                           'Convert missing values to 0\'s',
                           FALSE),
             bsTooltip(ns("chBdataNA20"), 
                       helpText.dataHist[["chBdataNA20"]],
                       placement = "top",
                       trigger = "hover"),
             
             
             
             # rescale data
             bsAlert("alertAnchorNegPresent"),
             
             selectInput(
               ns("selRescale"),
               p("Rescale data. ",
                 actionLink(ns("alLearnMoreRescale"), "Learn more")
               ),
               choices = c(
                 "no rescaling" = "noresc",
                 "z-score" = "zscore",
                 "log10(x)" = "log10x",
                 "log10(x+1)" = "log10xp1",
                 "winsorize" = "win"), 
               selected = "noresc"
             )
      )
      
    ),
    
    withSpinner(plotOutput(ns('plotHist'), 
               width = '100%')),
    tags$hr(),
    
    fluidRow(
      column(3,
             # trim data
             checkboxInput(ns('chBdataTrim'), 'Trim data'),
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
             checkboxInput(ns('chBdataClip'), 'Clip data'),
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
      ),
      column(3, 
             # Show data min/max
             tags$b("Extreme data points"),
             textOutput(ns('dataMin')),
             textOutput(ns('dataMax')),
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
    cat(file = stdout(), 'dataMax \n')

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
  
  # Set min/max numeric inputs for trimming and clipping to 
  # min/max values in the data
  observe({
    
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
    locDM = rescaledData()
    
    if (!is.null(locDM)) {
      locX = myMin(locDM, sig = SIGNIFDIGITSROUND, na.rm = T)
    } else {
      return(NULL)
    }
  })
  
  calcDataMax = reactive({
    locDM = rescaledData()
    
    if (!is.null(locDM)) {
      locX = myMax(locDM, sig = SIGNIFDIGITSROUND, na.rm = T)
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
        
        createAlert(session, "alertAnchorNegPresent", "alertNegPresent", title = "Warning",
                    content = helpText.server[["alertNegPresent"]], 
                    append = FALSE,
                    style = "warning")
        
      } else {
        closeAlert(session, "alertNegPresent")
      }
      
      locDM = log10(locDM)
    } else {
      closeAlert(session, "alertNegPresent")
    }
    
    # take log10(1+x) of data
    if (input$selRescale == "log10xp1") {
      if (sum(locDM <= -1, na.rm = T) > 0) {
        # Transform data points smaller or equal to -1 into NAs
        locDM[locDM <= -1] <- NA
        
        createAlert(session, "alertAnchorNegPresent", "alertNegPresent1", title = "Warning",
                    content = helpText.server[["alertNegPresent1"]], 
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
  
  # apply data filters from UI (clipping, trimming, rescaling)
  # return it to other modules
  dmReturn = reactive({ 
    cat(file = stdout(), 'tabHist:dmReturn\n')
    
    locDM = rescaledData()
    
    if (is.null(locDM)) {
      return(NULL)
    }
    
    # Data trimming
    # Data points outside of the range are set to NA.
    # This isn't affected by conversion to 0's above.
    if (input$chBdataTrim) {
      locDM[locDM < as.numeric(input$inDataTrimMin) & locDM != 0] <- NA
      
      # data points above a threshold are set to NA
      # this isn't affected by conversion to 0's above
      locDM[locDM > as.numeric(input$inDataTrimMax)] <- NA
    }
    
    
    # Data clipping
    # Data points outside of the range are set to range limits.
    if (input$chBdataClip) {
      locDM[locDM < as.numeric(input$inDataClipMin) &
              locDM != 0] <- input$inDataClipMin
      
      locDM[locDM > as.numeric(input$inDataClipMax)] <-
        input$inDataClipMax
    }
    
    return(locDM)
  })
  
  # Histogram ----
  
  giveMeNbins = reactive({
    
    locNbins = input$slHistBinN
    
    return(locNbins)
  }) %>% debounce(millis = MILLIS)
  
  output$plotHist <- renderPlot({
    cat(file = stdout(), 'tabHist:plotHist\n')
    
    loc.dm = rescaledData()
    
    validate(
      need(!is.null(loc.dm), "Nothing to plot. Load data first!")
    )
    
    if (is.null(loc.dm))
      return(NULL)
    
    # generate bins based on input$bins from ui.R
    locVecBins = seq(min(loc.dm, na.rm = T), 
                     max(loc.dm, na.rm = T), 
                     length.out = giveMeNbins() + 1)
    
    # Draw a histogram with vertical lines corresponding to:
    # trimming - red dashed
    # clipping - blue dotted
    hist(loc.dm, 
         breaks = locVecBins, 
         freq = TRUE,
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