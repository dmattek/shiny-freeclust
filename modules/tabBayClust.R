# NOT USED AT THE MOMENT
# The package bclust is not available from CRAN anymore; install manually to use this module

# Free-Clust: Shiny app for clustering data
# Author: Maciej Dobrzynski
#
# RShiny module for performing Bayesian clustering using bclust
# Use:
# in ui.R
# tabPanel(
#  'Hierarchical',
#  clustBayUI('tabBay'))
#
# in server.R
# callModule(clustBay, 'tabBay', dataMod)
# where dataMod is the output from a reactive function 
# that returns a dataset in wide format ready for clustering


require(gplots) # heatmap.2
require(RColorBrewer) # brewer.pal
require(d3heatmap) # interactive heatmap
require(bclust) # Bayesian clustering
require(shinyBS) # for tooltips
require(shinycssloaders) # for loader animations

helpText.clBay = c(alImportance = paste0("<p>Bayes weight (BW) calculated during clustering ",
                                         "reflect the importance of data points in the clustering. ",
                                         "The following labels are used to indicate the importance:",
                                         "<li>Black - data point not taken into account</li>",
                                         "<li>* - low, WF∈(0, 0.1]</li>",
                                         "<li>** - medium, WF∈(0.1, 0.5]</li>",
                                         "<li>*** - high, WF∈(0.5, 1.0]</li>",
                                         "</p><p>Nia and Davison (2012): ",
                                         "<i>High-Dimensional Bayesian Clustering with Variable Selection: The R Package bclust</i>; ",
                                         "Journal of Statistical Software 47(5).</p>"),
                   downClAss = "Download a CSV with cluster assignments to time series ID",
                   downDend = "Download an RDS file with dendrogram object. Read later with readRDS() function.",
                   alertNAsPresentClBay = "NAs (still) present in the dataset. bclust cannot continue.")

# UI ----
clustBayUI <- function(id, label = "Sparse Hierarchical CLustering") {
  ns <- NS(id)
  
  tagList(
    h4(
      "Bayesian clustering using ",
      a("bclust", 
        href = "https://cran.r-project.org/web/packages/bclust/index.html",
        title="External link",
        target = "_blank")
    ),
    p('The algorithm does not deal with missing values. Convert them to zeroes in the Histogram tab.'),
    p("Columns in the heatmap labeled according to their ",
      actionLink(ns("alImportance"), "importance.")),
    
    br(),
    fluidRow(
      column(4,
             uiOutput(ns('slNclustSlider'))
      ),
      column(
        3,
        bsAlert("alertAnchorClBayNAsPresent"),
        
      )
    ),
    
    checkboxInput(ns('chBplotStyle'),
                  'Adjust plot appearance',
                  FALSE),
    conditionalPanel(
      condition = "input.chBplotStyle",
      ns = ns,
      
      fluidRow(
        column(
          4, 
          checkboxInput(ns('chBdispGrid'), 
                        'Display grid lines', 
                        FALSE),
          conditionalPanel(
            condition = "input.chBdispGrid",
            ns = ns,
            
            sliderInput(
              ns('slGridColor'),
              'Shade of grey for grid lines',
              min = 0,
              max = 1,
              value = 0.6,
              step = .1,
              ticks = TRUE)
          )
        ),
        
        column(
          3,
          
          selectInput(
            ns("selectPalette"),
            label = "Heatmap\'s colour palette:",
            choices = l.col.pal,
            selected = 'Spectral'
          ),
          checkboxInput(ns('inPlotBayRevPalette'), 'Reverse colour palette', TRUE),
        ),
        
        column(
          3,
          selectInput(
            ns("selectPaletteDend"),
            label = "Dendrogram\'s colour palette",
            choices = l.col.pal.dend,
            selected = 'Color Blind'
          ),
          checkboxInput(ns('selectPlotBayDend'), 'Plot dendrogram and re-order samples', TRUE)
          
        )
      ),
      
      fluidRow(
        
        column(
          2,
          numericInput(
            ns('inPlotBayHmFontX'),
            'Font size row labels',
            1,
            min = 0,
            width = 100,
            step = 0.1
          )
        ),
        column(
          2,
          numericInput(
            ns('inPlotBayHmFontY'),
            'Font size column labels',
            1,
            min = 0,
            width = 100,
            step = 0.1
          )
        ),
        column(3,
               numericInput(
                 ns('inPlotHeight'),
                 'Display plot height',
                 value = 800,
                 min = 100,
                 step = 100
               )
        ),
        column(3,
               numericInput(
                 ns('inPlotWidth'),
                 'Display plot width',
                 value = 800,
                 min = 100,
                 step = 100
               )
        )
      )
    ),
    
    checkboxInput(ns('chBdownload'),
                  'Download plot or data',
                  FALSE),
    conditionalPanel(
      condition = "input.chBdownload",
      ns = ns,
      
      fluidRow(
        column(3,
               downloadButton(ns('downClAssBay'), 'Cluster assignments'),
               bsTooltip(ns("downClAssBay"),
                         helpText.clBay[["downClAss"]],
                         placement = "top",
                         trigger = "hover",
                         options = NULL),
        ),
        
        column(3,
               downloadButton(ns('downDendBay'), 'Dendrogram object'),
               bsTooltip(ns("downDendBay"),
                         helpText.clBay[["downDend"]],
                         placement = "top",
                         trigger = "hover",
                         options = NULL)
        ),
      ),
      downPlotUI(ns('downPlotBayHM'), "")
    ),
    
    checkboxInput(ns('inPlotBayInteractive'), 'Interactive Plot',  value = FALSE),
    uiOutput(ns("plotUI"))
    
  )
}

# SERVER ----
clustBay <- function(input, output, session, dataMod) {
  
  ns = session$ns
  
  # Return the number of clusters from the slider 
  # and delay by a constant in milliseconds defined in auxfunc.R
  returnNclust = reactive({
    return(input$slNclust)
  }) %>% debounce(MILLIS)
  
  # Perform Bayesian clustering
  calcBclust <- reactive({
    cat(file = stdout(), 'tabBay:calcBclust\n')
    
    locDM = dataMod()
    
    if (is.null(locDM))
      return(NULL)
    
    # Throw some warnings if NAs present in the dataset.
    # DTW cannot compute distance when NA's are preset.
    # Other distance measures can be calculated but caution is required with interpretation.
    # NAs in the wide format can result from explicit NAs in the measurment column or
    # from missing rows that cause NAs to appear when convertinf from long to wide (dcast)
    if(sum(is.na(locDM)) > 0) {
      createAlert(session, "alertAnchorClBayNAsPresent", "alertNAsPresentClBay", title = "Error",
                  content = helpText.clBay[["alertNAsPresentClBay"]], 
                  append = FALSE,
                  style = "danger")
      
      return(NULL)
      
    } else {
      closeAlert(session, 'alertNAsPresentClBay')
    }
    
    locClBay = bclust::bclust(locDM, 
                              transformed.par = c(0, -50, log(16), 0, 0, 0))
    
    return(locClBay)
  })
  
  # Calculate dendrogram
  calcDend <- reactive({
    cat(file = stdout(), 'tabBay:calcDend\n')
    
    locBclust = calcBclust()
    if (is.null(locBclust))
      return(NULL)
    
    locDend <- as.dendrogram(locBclust)
    
    return(locDend)
    
  })
  
  calcVarImpBclust <- reactive({
    cat(file = stdout(), 'tabBay:calcVarImpBclust\n')
    
    locBclust = calcBclust()
    if (is.null(locBclust))
      return(NULL)
    
    locImp = bclust::imp(locBclust)
    if (is.null(locImp))
      return(NULL)
    
    return(locImp[["var"]])
  })
  
  
  output$slNclustSlider = renderUI({
    ns <- session$ns
    
    locDM = dataMod()
    if (is.null(locDM))
      return(NULL)
    
    locBclust = calcBclust()
    if (is.null(locBclust))
      return(NULL)
    
    sliderInput(
      ns('slNclust'),
      'Number of dendrogram branches to cut (default: optimal from bclust)',
      min = 1,
      max = round(nrow(locDM) * 0.3),
      value = locBclust[["optim.clustno"]],
      step = 1,
      ticks = TRUE,
      round = TRUE
    )
  })
  
  # download a list of IDs with cluster assignments
  output$downClAssBay <- downloadHandler(
    filename = function() {
      'clust_bayes_data.csv'
    },
    
    content = function(file) {
      fwrite(x = myGetDataCl(calcDend(), 
                             input$slNclust), 
             file = file, 
             row.names = FALSE)    }
  )
  
  # download an RDS file with dendrogram objet
  output$downDendBay <- downloadHandler(
    filename = function() {
      'clust_bayes_dend.rds'
    },
    
    content = function(file) {
      saveRDS(object = calcDend(), file = file)
    }
  )
  
  
  # Bayesian clustering - render plot
  output$outPlotHier <- renderPlot({
    plotHier()
  })
  
  createFnameHeatMap = reactive({
    
    return('clust_bayes.png')
  })
  
  # Download the plot as a PNG
  callModule(downPlot, "downPlotBayHM", createFnameHeatMap, plotBayHm)
  
  # Plot heatmap
  plotBayHm <- function() {
    cat(file = stdout(), 'tabBay:plotBayHm\n')
    
    locDM = dataMod()
    locDend = calcDend()
    locVarImp = calcVarImpBclust()

    validate(
      need(!is.null(locDM), "Nothing to plot. Load data first!"),
      need(!is.null(locDend), "Did not create dendrogram"),
      need(!is.null(locVarImp), "Did not calculate variable importance")
    )

    # Prepare color labels for row-side annotations
    col_labels <- get_leaves_branches_col(locDend)
    col_labels <- col_labels[order(order.dendrogram(locDend))]
    
    # Prepare color palette for the heatmap
    if (input$inPlotBayRevPalette)
      locMyPal <-
      rev(colorRampPalette(brewer.pal(9, input$selectPalette))(n = 99))
    else
      locMyPal <-
      colorRampPalette(brewer.pal(9, input$selectPalette))(n = 99)
    
    # number of clusters at which dendrogram is cut
    
    # yes/no for plotting dendrogram on the left hand side of the heatmap
    if (input$selectPlotBayDend) {
      assign("locVarTmp1", locDend)
      locVarTmp2 = "row"
    } else {
      assign("locVarTmp1", FALSE)
      locVarTmp2 = "none"
    }
    
    # Prepare column labels for the heatmap
    # They're coloured and prefixed with variable importance
    locColumnNames = paste0(ifelse(locVarImp < 0, "- ",
                                   ifelse(
                                     locVarImp < quantile(locVarImp, 0.25), "",
                                     ifelse(locVarImp < quantile(locVarImp, 0.5), "* ", 
                                            ifelse(locVarImp < quantile(locVarImp, 0.75), "** ", "*** "))
                                   )), colnames(locDM))
    
    
    heatmap.2(
      locDM,
      Colv = "NA",
      Rowv = locVarTmp1,
      srtCol = 45,
      dendrogram = locVarTmp2,
      trace = "none",
      margins = c(10,10),
      key = T,
      col = locMyPal,
      denscol = "black",
      density.info = "density",
      RowSideColors = col_labels,
      colRow = col_labels,
      labCol = locColumnNames,
      sepcolor = if (input$chBdispGrid) grey(input$slGridColor) else NULL,
      colsep = if (input$chBdispGrid) 1:ncol(locDM) else NULL,
      rowsep = if (input$chBdispGrid) 1:nrow(locDM) else NULL,
      cexRow = input$inPlotBayHmFontX,
      cexCol = input$inPlotBayHmFontY,
      main = "Bayesian Clustering (bclust)"
    )    
}
  
  
  plotBayImp <- function() {
    cat(file = stdout(), 'tabBay:plotBayImp\n')
    
    locDM = dataMod()
    if (is.null(locDM))
      return(NULL)
    
    locBclust = calcBclust()
    if (is.null(locBclust))
      return(NULL)
    
    #cat(imp(locBclust)$var)
    
    viplot(
      imp(locBclust)$var,
      xlab = colnames(locDM),
      xlab.srt = 90,
      xlab.cex = input$inPlotBayHmFontY,
      main = '\nVariable importance\n'
    )
  }
  
  
  
  output$outPlotBayHm <- renderPlot({
    plotBayHm()
  })
  
  output$plotBayInt <- renderD3heatmap({
    cat(file = stdout(), 'tabBay:plotBayInt\n')
    
    locDM = dataMod()
    locDend <- calcDend()
    locVarImp = calcVarImpBclust()
    
    validate(
      need(!is.null(locDM), "Nothing to plot. Load data first!"),
      need(!is.null(locDend), "Did not create dendrogram"),
      need(!is.null(locVarImp), "Did not calculate variable importance")
    )
    
    # Prepare color palette for the heatmap
    if (input$inPlotBayRevPalette)
      locMyPal = rev(
        colorRampPalette(
          brewer.pal(9, 
                     input$selectPalette))(n = 99))
    else
      locMyPal = colorRampPalette(
        brewer.pal(9, 
                   input$selectPalette))(n = 99)
    
    # yes/no for plotting dendrogram on the left hand side of the heatmap
    if (input$selectPlotBayDend) {
      assign("locVarTmp1", locDend)
      locVarTmp2 = "row"
    } else {
      assign("locVarTmp1", FALSE)
      locVarTmp2 = "none"
    }
    
    # Prepare column labels for the heatmap
    # They're coloured and prefixed with variable importance
    locColumnNames = paste0(ifelse(locVarImp < 0, "- ",
                                   ifelse(
                                     locVarImp < quantile(locVarImp, 0.25), "",
                                     ifelse(locVarImp < quantile(locVarImp, 0.5), "* ", 
                                            ifelse(locVarImp < quantile(locVarImp, 0.75), "** ", "*** "))
                                   )), colnames(locDM))
    
    d3heatmap(
      locDM,
      Rowv = locVarTmp1,
      dendrogram = locVarTmp2,
      trace = "none",
      revC = FALSE,
      margins = c(10,10),
      colors = locMyPal,
      cexRow = input$inPlotBayHmFontY * 10,
      cexCol = input$inPlotBayHmFontX * 10,
      show_grid = TRUE,
      xaxis_height = 10,
      yaxis_width = 10,
      labRow = rownames(locDM),
      labCol = locColumnNames
    )
  })
  
  output$plotUI <- renderUI({
    ns <- session$ns
    
    if (input$inPlotBayInteractive)
      d3heatmapOutput(ns("plotBayInt"), 
                      height = paste0(input$inPlotHeight, "px"), 
                      width = paste0(input$inPlotWidth, "px"))
    else {
      withSpinner(plotOutput(ns('outPlotBayHm'), 
                             height = paste0(input$inPlotHeight, "px"), 
                             width = paste0(input$inPlotWidth, "px")))
    }
  })
  
  addPopover(session, 
             ns("alImportance"),
             title = "Variable importance",
             content = helpText.clBay[["alImportance"]],
             trigger = "click")
}
