#
# Free-Clust: Shiny app for clustering datas data
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


helpText.dataHist = c(alLearnMore = paste0("<p>Avoid long tails or strong assymetries in the histogram ",
                                       "by converting your data to Z-scores, taking logarithm of data, ",
                                       "or by removing the outliers.</p>")
                      )
# UI
dataHistUI <- function(id, label = "Histogram") {
  ns <- NS(id)
  
  tagList(
    h4('Data histogram'),
    p("Overview of your data. ",
      actionLink(ns("alLearnMore"), "Learn more")
    ),
    
    br(),
    sliderInput(
      ns('slHistBinN'),
      'Set the number of histogram bins',
      min = 1,
      max = 100,
      value = 10,
      step = 1
    ),
    plotOutput(ns('plotHist'), width = '100%')
  )
}

# SERVER
dataHist <- function(input, output, session, dataMod) {
  
  ns <- session$ns
  
  giveMeNbins = reactive({
    
    locNbins = input$slHistBinN
    
    return(locNbins)
  }) %>% debounce(millis = MILLIS)
  
  output$plotHist <- renderPlot({
    cat(file = stderr(), 'plotHist \n')
    
    loc.dm = dataMod()
    
    validate(
      need(!is.null(loc.dm), "Nothing to plot. Load data first!")
    )
    
    if (is.null(loc.dm))
      return(NULL)
    
    # generate bins based on input$bins from ui.R
    locVecBins = seq(min(loc.dm, na.rm = T), 
                     max(loc.dm, na.rm = T), 
                     length.out = giveMeNbins() + 1)
    
      hist(loc.dm, 
           breaks = locVecBins, 
           freq = TRUE,
           main = 'Histogram of data',
           xlab = 'Values',
           col = 'darkgray', 
           border = 'white')
  })
  
  # Pop-overs ----
  addPopover(session, 
             ns("alLearnMore"),
             title = "Data histogram",
             content = helpText.dataHist[["alLearnMore"]],
             trigger = "click")
  
  
}