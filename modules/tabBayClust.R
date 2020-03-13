#
# Free-Clust: Shiny app for clustering data
# Author: Maciej Dobrzynski
#
# RShiny module for performing Bayesian clustering using bclust
# Use:
# in ui.R
# tabPanel(
#  'Hierarchical',
#  clustBayUI('TabClustBay'))
#
# in server.R
# callModule(clustBay, 'TabClustBay', dataMod)
# where dataMod is the output from a reactive function 
# that returns a dataset in wide format ready for clustering


require(gplots) # heatmap.2
require(dendextend) # color_branches
require(RColorBrewer) # brewer.pal
require(d3heatmap) # interactive heatmap
require(bclust) # Bayesian clustering
require(shinyBS) # for tooltips
require(shinycssloaders) # for loader animations

helpText.clBayes = c(alImportance = paste0("<p>Bayes weight (BW) calculated during clustering ",
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
                     downDend = "Download an RDS file with dendrogram object. Read later with readRDS() function.")

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
      column(3,
             checkboxInput(ns('selectPlotBayDend'),
                           'Plot dendrogram and re-order samples', TRUE),
             uiOutput(ns('slNclustSlider'))
      ),
      column(3,
             selectInput(
               ns("selectPlotBayPalette"),
               label = "Heatmap\'s colour palette:",
               choices = l.col.pal,
               selected = 'Spectral'
             ),
             selectInput(
               ns("selectPlotBayPaletteDend"),
               label = "Dendrogram\'s colour palette",
               choices = l.col.pal.dend,
               selected = 'Color Blind'
             ),
             checkboxInput(ns('inPlotBayRevPalette'), 'Reverse colour palette', TRUE),
             checkboxInput(ns('selectPlotBayKey'), 'Plot colour key', TRUE)
      ),
      column(3,
             checkboxInput(ns('inDispGrid'), 
                           'Display grid lines', 
                           FALSE),
             uiOutput(ns('inGridColorUI'))
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
        2,
        numericInput(
          ns('inPlotBayHmMarginX'),
          'Bottom margin',
          10,
          min = 1,
          width = 100
        )
      ),
      column(
        2,
        numericInput(
          ns('inPlotBayHmMarginY'),
          'Right margin',
          10,
          min = 1,
          width = 100
        )
      ),
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
      column(2,
             numericInput(
               ns('inPlotHeight'),
               'Display plot height',
               value = 1000,
               min = 100,
               step = 100
             ),
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
        column(4,
               
               downloadButton(ns('downClAssBay'), 'Cluster assignments'),
               bsTooltip(ns("downClAssBay"),
                         helpText.clBayes[["downClAss"]],
                         placement = "top",
                         trigger = "hover",
                         options = NULL)
        ),
        
        column(4,
               
               downloadButton(ns('downDendBay'), 'Dendrogram object'),
               bsTooltip(ns("downDendBay"),
                         helpText.clBayes[["downDend"]],
                         placement = "top",
                         trigger = "hover",
                         options = NULL))
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
  
  calcBclust <- reactive({
    cat(file = stdout(), 'calcBclust \n')
    
    locDM = dataMod()
    if (is.null(locDM))
      return(NULL)
    
    bclust(locDM, transformed.par = c(0, -50, log(16), 0, 0, 0))
  })
  
  calcDend <- reactive({
    cat(file = stdout(), 'calcDend \n')
    
    d.bclus = calcBclust()
    if (is.null(d.bclus))
      return(NULL)
    
    # number of clusters at which dendrogram is cut
    loc.k = returnNclust()
    
    # make a palette with the amount of colours equal to the number of clusters
    loc.col = ggthemes::tableau_color_pal(input$selectPlotBayPaletteDend)(n = loc.k)
    
    dend <- as.dendrogram(d.bclus)
    dend <- color_branches(dend, 
                           col = loc.col,
                           k = loc.k)
    
    return(dend)

  })
  
  userVarImpBclus <- reactive({
    cat(file = stdout(), 'userVarImpBclus \n')
    
    d.bclus = calcBclust()
    if (is.null(d.bclus))
      return(NULL)
    
    return(imp(d.bclus)$var)
  })
  
  
  output$slNclustSlider = renderUI({
    ns <- session$ns

    locDM = dataMod()
    if (is.null(locDM))
      return(NULL)
    
    loc.d.bclus = calcBclust()
    if (is.null(loc.d.bclus))
      return(NULL)
    
    sliderInput(
      ns('slNclust'),
      'Number of dendrogram branches to cut (default: optimal from bclust)',
      min = 1,
      max = round(nrow(locDM) * 0.3),
      value = loc.d.bclus$optim.clustno,
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
  
  callModule(downPlot, "downPlotBayHM", createFnameHeatMap, plotBayHm)
    
  plotBayHm <- function() {
    cat(file = stdout(), 'plotBayHm \n')
    
    locDM = dataMod()
    loc.dend <- calcDend()
    loc.var.imp = imp(calcBclust())$var
    
    validate(
      need(!is.null(locDM), "Nothing to plot. Load data first!"),
      need(!is.null(loc.var.imp), "Did not cluster"),
      need(!is.null(loc.dend), "Did not create dendrogram")
    )
    
    col_labels <- get_leaves_branches_col(loc.dend)
    col_labels <- col_labels[order(order.dendrogram(loc.dend))]
    
    if (input$inPlotBayRevPalette)
      my_palette <-
      rev(colorRampPalette(brewer.pal(9, input$selectPlotBayPalette))(n = 99))
    else
      my_palette <-
      colorRampPalette(brewer.pal(9, input$selectPlotBayPalette))(n = 99)
    
    if (input$selectPlotBayDend) {
      assign("var.tmp.1", loc.dend)
      var.tmp.2 = "row"
    } else {
      assign("var.tmp.1", FALSE)
      var.tmp.2 = "none"
    }
    
    loc.colnames = paste0(ifelse(loc.var.imp < 0, "- ",
                                 ifelse(
                                   loc.var.imp < quantile(loc.var.imp, 0.25), "",
                                   ifelse(loc.var.imp < quantile(loc.var.imp, 0.5), "* ", 
                                          ifelse(loc.var.imp < quantile(loc.var.imp, 0.75), "** ", "*** "))
                                 )), colnames(locDM))
    
    loc.colcol   = ifelse(loc.var.imp < 0, "blue",
                          ifelse(
                            loc.var.imp < quantile(loc.var.imp, 0.25), "black",
                            ifelse(loc.var.imp < quantile(loc.var.imp, 0.5), "green", 
                                   ifelse(loc.var.imp < quantile(loc.var.imp, 0.75), "orange", "red"))
                          ))
    
    
    heatmap.2(
      locDM,
      Colv = "NA",
      Rowv = var.tmp.1,
      srtCol = 90,
      dendrogram = var.tmp.2,
      trace = "none",
      key = input$selectPlotBayKey,
      margins = c(input$inPlotBayHmMarginX, input$inPlotBayHmMarginY),
      col = my_palette,
      na.col = grey(input$inPlotBayHmNAcolor),
      denscol = "black",
      density.info = "density",
      RowSideColors = col_labels,
      colRow = col_labels,
      colCol = loc.colcol,
      labCol = loc.colnames,
      sepcolor = if (input$inDispGrid) grey(input$inPlotBayHmGridColor) else NULL,
      colsep = if (input$inDispGrid) 1:ncol(locDM) else NULL,
      rowsep = if (input$inDispGrid) 1:nrow(locDM) else NULL,
      cexRow = input$inPlotBayHmFontX,
      cexCol = input$inPlotBayHmFontY,
      main = "Bayesian Clustering (bclust)"
    )
  }
  
  output$inGridColorUI <- renderUI({
    ns <- session$ns
    
    if(input$inDispGrid) {
      sliderInput(
        ns('inPlotBayHmGridColor'),
        'Shade of grey for grid lines',
        min = 0,
        max = 1,
        value = 0.6,
        step = .1,
        ticks = TRUE)
    }
  })
  
  
  plotBayImp <- function() {
    cat(file = stdout(), 'plotBayImp \n')
    
    locDM = dataMod()
    if (is.null(locDM))
      return(NULL)
    
    loc.d.bclus = calcBclust()
    if (is.null(loc.d.bclus))
      return(NULL)
    
    #cat(imp(loc.d.bclus)$var)
    
    viplot(
      imp(loc.d.bclus)$var,
      xlab = colnames(locDM),
      xlab.srt = 90,
      xlab.mar = input$inPlotBayHmMarginX,
      xlab.cex = input$inPlotBayHmFontY,
      main = '\nVariable importance\n'
    )
  }
  

  
  output$outPlotBayHm <- renderPlot({
    plotBayHm()
  })
  
  output$plotBayInt <- renderD3heatmap({
    cat(file = stdout(), 'plotBayInt \n')
    
    locDM = dataMod()
    if (is.null(locDM))
      return(NULL)
    
    loc.dend = calcDend()
    if (is.null(loc.dend))
      return(NULL)
    
    loc.var.imp = imp(calcBclust())$var
    if (is.null(loc.var.imp))
      return(NULL)
    
    col_labels <- get_leaves_branches_col(loc.dend)
    col_labels <- col_labels[order(order.dendrogram(loc.dend))]
    
    if (input$inPlotBayRevPalette)
      my_palette <-
      rev(colorRampPalette(brewer.pal(9, input$selectPlotBayPalette))(n = 99))
    else
      my_palette <-
      colorRampPalette(brewer.pal(9, input$selectPlotBayPalette))(n = 99)
    
    if (input$selectPlotBayDend) {
      assign("var.tmp.1", loc.dend)
      var.tmp.2 = "row"
    } else {
      assign("var.tmp.1", FALSE)
      var.tmp.2 = "none"
    }
    
    loc.colnames = paste0(ifelse(loc.var.imp < 0, "- ",
                                 ifelse(
                                   loc.var.imp < quantile(loc.var.imp, 0.25), "",
                                   ifelse(loc.var.imp < quantile(loc.var.imp, 0.5), "* ", 
                                          ifelse(loc.var.imp < quantile(loc.var.imp, 0.75), "** ", "*** "))
                                 )), colnames(locDM))
    
    d3heatmap(
      locDM,
      Rowv = var.tmp.1,
      dendrogram = var.tmp.2,
      trace = "none",
      revC = FALSE,
      margins = c(input$inPlotBayHmMarginX, input$inPlotBayHmMarginY),
      colors = my_palette,
      na.col = grey(input$inPlotBayNAcolor),
      cexRow = input$inPlotBayHmFontY,
      cexCol = input$inPlotBayHmFontX,
      xaxis_height = input$inPlotBayHmMarginX,
      yaxis_width = input$inPlotBayHmMarginY,
      show_grid = TRUE,
      labRow = rownames(locDM),
      labCol = loc.colnames
    )
  })
  
  output$plotUI <- renderUI({
    ns <- session$ns
    
    if (input$inPlotBayInteractive)
      withSpinner(d3heatmapOutput(ns("plotBayInt"), 
                                  height = paste0(input$inPlotHeight, "px"), 
                                  width = paste0(input$inPlotWidth, "px")))
    else {
      withSpinner(plotOutput(ns('outPlotBayHm'), 
                             height = paste0(input$inPlotHeight, "px"), 
                             width = paste0(input$inPlotWidth, "px")))
    }
  })
  
  addPopover(session, 
             ns("alImportance"),
             title = "Variable importance",
             content = helpText.clBayes[["alImportance"]],
             trigger = "click")
  }
