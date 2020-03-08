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
                                              "<li><p, style=\"color:DodgerBlue;\">* - low, WF∈(0, 0.1]</p></li>",
                                              "<li><p, style=\"color:MediumSeaGreen;\">** - medium, WF∈(0.1, 0.5]</p></li>",
                                              "<li><p, style=\"color:Tomato;\">*** - high, WF∈(0.5, 1.0]</p></li>",
                                              "</p><p>Nia and Davison (2012): ",
                                              "<i>High-Dimensional Bayesian Clustering with Variable Selection: The R Package bclust</i>; ",
                                              "Journal of Statistical Software 47(5).</p>"))

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
    p('The algorithm does not deal with missing values. Use conversion to zeroes in the right panel.'),
    p("Columns in the heatmap labeled according to their ",
      actionLink(ns("alImportance"), "importance.")),
    
    br(),
    fluidRow(
      column(3,
             checkboxInput(ns('selectPlotBayDend'),
                           'Plot dendrogram and re-order samples', TRUE),
             uiOutput(ns('inPlotBayHmNclustSlider'))
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
                           TRUE),
             uiOutput(ns('inGridColorUI'))
      )
    ),
    
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
    ),
    br(),
    
    downPlotUI(ns('downPlotBayHM')),
    
    
    br(),
    checkboxInput(ns('inPlotBayInteractive'), 'Interactive Plot?',  value = FALSE),
    uiOutput(ns("plotBayInt_ui"))
  
  )
}

# SERVER ----
clustBay <- function(input, output, session, dataMod) {
  
  ns = session$ns
  
  userFitBclus <- reactive({
    cat(file = stdout(), 'userFitBclus \n')
    
    loc.dm = dataMod()
    if (is.null(loc.dm))
      return(NULL)
    
    bclust(loc.dm, transformed.par = c(0, -50, log(16), 0, 0, 0))
  })
  
  userDendBclus <- reactive({
    cat(file = stdout(), 'userDendBclus \n')
    
    d.bclus = userFitBclus()
    if (is.null(d.bclus))
      return(NULL)
    
    # number of clusters at which dendrogram is cut
    loc.k = input$inPlotBayHmNclust
    
    # make a palette with the amount of colours equal to the number of clusters
    #loc.col = get(input$selectPlotHierPaletteDend)(n = loc.k)
    loc.col = ggthemes::tableau_color_pal(input$selectPlotBayPaletteDend)(n = loc.k)
    
    dend <- as.dendrogram(d.bclus)
    #    dend <- color_branches(dend, k = d.bclus$optim.clustno)
    dend <- color_branches(dend, 
                           col = loc.col,
                           k = loc.k)
    #    browser()
  })
  
  userVarImpBclus <- reactive({
    cat(file = stdout(), 'userVarImpBclus \n')
    
    d.bclus = userFitBclus()
    if (is.null(d.bclus))
      return(NULL)
    
    return(imp(d.bclus)$var)
  })
  
  
  output$inPlotBayHmNclustSlider = renderUI({
    ns <- session$ns

    loc.dm = dataMod()
    if (is.null(loc.dm))
      return(NULL)
    
    loc.d.bclus = userFitBclus()
    if (is.null(loc.d.bclus))
      return(NULL)
    
    sliderInput(
      ns('inPlotBayHmNclust'),
      'Number of dendrogram branches to cut (default: optimal from bclust)',
      min = 1,
      max = nrow(loc.dm),
      value = loc.d.bclus$optim.clustno,
      step = 1,
      ticks = TRUE,
      round = TRUE
    )
  })
  

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
    
    loc.dm = dataMod()
    if (is.null(loc.dm))
      return(NULL)
    
    loc.dend = userDendBclus()
    if (is.null(loc.dend))
      return(NULL)
    
    loc.var.imp = imp(userFitBclus())$var
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
                                 )), colnames(loc.dm))
    
    loc.colcol   = ifelse(loc.var.imp < 0, "blue",
                          ifelse(
                            loc.var.imp < quantile(loc.var.imp, 0.25), "black",
                            ifelse(loc.var.imp < quantile(loc.var.imp, 0.5), "green", 
                                   ifelse(loc.var.imp < quantile(loc.var.imp, 0.75), "orange", "red"))
                          ))
    
    
    heatmap.2(
      loc.dm,
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
      colsep = if (input$inDispGrid) 1:ncol(loc.dm) else NULL,
      rowsep = if (input$inDispGrid) 1:nrow(loc.dm) else NULL,
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
    
    loc.dm = dataMod()
    if (is.null(loc.dm))
      return(NULL)
    
    loc.d.bclus = userFitBclus()
    if (is.null(loc.d.bclus))
      return(NULL)
    
    #cat(imp(loc.d.bclus)$var)
    
    viplot(
      imp(loc.d.bclus)$var,
      xlab = colnames(loc.dm),
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
    
    loc.dm = dataMod()
    if (is.null(loc.dm))
      return(NULL)
    
    loc.dend = userDendBclus()
    if (is.null(loc.dend))
      return(NULL)
    
    loc.var.imp = imp(userFitBclus())$var
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
                                 )), colnames(loc.dm))
    
    d3heatmap(
      loc.dm,
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
      labRow = rownames(loc.dm),
      labCol = loc.colnames
    )
  })
  
  output$plotBayInt_ui <- renderUI({
    ns <- session$ns
    
    if (input$inPlotBayInteractive)
      d3heatmapOutput(ns("plotBayInt"), height = paste0(input$inPlotHeight, "px"), width = paste0(input$inPlotWidth, "px"))
    else {
      plotOutput(ns('outPlotBayHm'), height = paste0(input$inPlotHeight, "px"), width = paste0(input$inPlotWidth, "px"))
    }
  })
  
  addPopover(session, 
             ns("alImportance"),
             title = "Variable importance",
             content = helpText.clBayes[["alImportance"]],
             trigger = "click")
  }
