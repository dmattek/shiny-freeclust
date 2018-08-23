# RShiny module for performing sparse hierarchical clustering using sparcl
# Use:
# in ui.R
# tabPanel(
#  'Hierarchical',
#  clustHierUI('TabClustHier'))
#
# in server.R
# callModule(clustHier, 'TabClustHier', dataMod)
# where dataMod is the output from a reactive function that returns dataset ready for clustering


require(gplots) # heatmap.2
require(dendextend) # color_branches
require(RColorBrewer) # brewer.pal
require(d3heatmap) # interactive heatmap
require(sparcl) # sparse hierarchical and k-means

s.cl.spar.linkage = c("average",
                      "complete", 
                      "single",
                      "centroid")

s.cl.spar.dist = c("squared.distance","absolute.value")

l.col.pal = list(
  "Spectral" = 'Spectral',
  "White-Orange-Red" = 'OrRd',
  "Yellow-Orange-Red" = 'YlOrRd',
  "Reds" = "Reds",
  "Oranges" = "Oranges",
  "Greens" = "Greens",
  "Blues" = "Blues"
)

# UI
clustHierSparUI <- function(id, label = "Sparse Hierarchical CLustering") {
  ns <- NS(id)
  
  tagList(
    h4(
      "Sparse hierarchical clustering using ",
      a("sparcl", href = "https://cran.r-project.org/web/packages/sparcl/")
    ),
    p(
      'Column labels in the heat-map are additionally labeled according to their weight factor (\"importance\"):'
    ),
    tags$ol(
      tags$li("Black - not taken into account"),
      tags$li("Blue with \"*\" - low importance (weight factor in (0, 0.1])"),
      tags$li("Green with \"**\" - medium importance (weight factor in (0.1, 0.5])"),
      tags$li("Red with \"***\" - high importance (weight factor in (0.5, 1.0])")
    ),
    br(),
    fluidRow(
      column(
        6,
        selectInput(
          ns("selectPlotHierSparLinkage"),
          label = ("Select linkage method:"),
          choices = list(
            "Average" = 1,
            "Complete" = 2,
            "Single" = 3,
            "Centroid" = 4
          ),
          selected = 1
        ),
        selectInput(
          ns("selectPlotHierSparDist"),
          label = ("Select type of dissimilarity measure:"),
          choices = list("Squared Distance" = 1,
                         "Absolute Value" = 2),
          selected = 1
        ),
        checkboxInput(ns('selectPlotHierSparDend'), 'Plot dendrogram and re-order samples', TRUE),
        sliderInput(
          ns('inPlotHierSparNclust'),
          '#dendrogram branches to colour',
          min = 1,
          max = 10,
          value = 1,
          step = 1,
          ticks = TRUE,
          round = TRUE
        ),
        checkboxInput(ns('selectPlotHierSparKey'), 'Plot colour key', TRUE),
        downloadButton(ns('downCellClSpar'), 'Download CSV with cluster associations')
      ),
      
      column(
        6,
        selectInput(
          ns("selectPlotHierSparPalette"),
          label = "Select colour palette:",
          choices = l.col.pal,
          selected = 'Spectral'
        ),
        checkboxInput(ns('inPlotHierSparRevPalette'), 'Reverse colour palette', TRUE),
        
        sliderInput(
          ns('inPlotHierSparNAcolor'),
          'Shade of grey for NA values (0 - black, 1 - white)',
          min = 0,
          max = 1,
          value = 0.8,
          step = .1,
          ticks = TRUE
        ),
        
        checkboxInput(ns('inDispGrid'), 
                      'Display grid lines', 
                      TRUE),
        uiOutput(ns('inGridColorUI'))
      )
    ),
    
    fluidRow(column(
      12,
      checkboxInput(ns('inHierSparAdv'),
                    'Advanced options',
                    FALSE),
      fluidRow(column(6,
                      uiOutput(
                        ns('uiPlotHierSparNperms')
                      )),
               column(6,
                      uiOutput(
                        ns('uiPlotHierSparNiter')
                      )))
    )),
    
    fluidRow(
      column(
        2,
        numericInput(
          ns('inPlotHierSparMarginX'),
          'Margin below x-axis',
          10,
          min = 1,
          width = 100
        )
      ),
      column(
        2,
        numericInput(
          ns('inPlotHierSparMarginY'),
          'Margin right of y-axis',
          10,
          min = 1,
          width = 100
        )
      ),
      column(
        2,
        numericInput(
          ns('inPlotHierSparFontX'),
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
          ns('inPlotHierSparFontY'),
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
    
    
    downPlotUI(ns('downPlotHierSparPNG'), "Download PNG"),
    
    br(),
    checkboxInput(ns('inPlotHierSparInteractive'), 'Interactive Plot?',  value = FALSE),
    uiOutput(ns("plotHierSparInt_ui"))
  )
}

# SERVER
clustHierSpar <- function(input, output, session, dataMod) {

  # UI for advanced options
  output$uiPlotHierSparNperms = renderUI({
    ns <- session$ns
    
    if (input$inHierSparAdv)
      sliderInput(
        ns('inPlotHierSparNperms'),
        'Number of permutations',
        min = 1,
        max = 20,
        value = 10,
        step = 1,
        ticks = TRUE
      )
  })
  
  # UI for advanced options
  output$uiPlotHierSparNiter = renderUI({
    ns <- session$ns
    
    if (input$inHierSparAdv)
      sliderInput(
        ns('inPlotHierSparNiter'),
        'Number of iterations',
        min = 1,
        max = 50,
        value = 15,
        step = 1,
        ticks = TRUE
      )
  })
  
  
  userFitHierSpar <- reactive({
    cat(file = stderr(), 'userFitHierSpar \n')
    
    loc.dm = dataMod()
    if (is.null(loc.dm))
      return(NULL)
    
    perm.out <- HierarchicalSparseCluster.permute(
      loc.dm,
      wbounds = NULL,
      nperms = ifelse(input$inHierSparAdv, input$inPlotHierSparNperms, 10),
      dissimilarity = s.cl.spar.dist[as.numeric(input$selectPlotHierSparDist)]
    )
    
    sparsehc <- HierarchicalSparseCluster(
      dists = perm.out$dists,
      wbound = perm.out$bestw,
      niter = ifelse(input$inHierSparAdv, input$inPlotHierSparNiter, 15),
      method = s.cl.spar.linkage[as.numeric(input$selectPlotHierSparLinkage)],
      dissimilarity = s.cl.spar.dist[as.numeric(input$selectPlotHierSparDist)]
    )
    return(sparsehc)
  })

  
  userFitDendHierSpar <- reactive({
    cat(file = stderr(), 'userFitDendHierSpar \n')
    
    sparsehc = userFitHierSpar()
    if (is.null(sparsehc))
      return(NULL)
    
    dend <- as.dendrogram(sparsehc$hc)
    dend <- color_branches(dend, k = input$inPlotHierSparNclust)
    
    return(dend)
  })
  
  # return all IDs (created in dataMod)
  # used when saving cluster associations in sparse hierarchical
  # sparsehc doesn't return original rownames after clustering
  getDataIDs <- reactive({
    cat(file = stderr(), 'getDataIDs\n')
    loc.m = dataMod()
    
    if (is.null(loc.m))
      return(NULL)
    else
      return(rownames(loc.m))
  })
  
  # download a list of IDs with cluster assignments
  output$downCellClSpar <- downloadHandler(
    filename = function() {
      paste0('clust_hierchSpar_data_',
             s.cl.spar.dist[as.numeric(input$selectPlotHierSparDist)],
             '_',
             s.cl.spar.linkage[as.numeric(input$selectPlotHierSparLinkage)], '.csv')
    },
    
    content = function(file) {
      write.csv(x = getDataClSpar(userFitDendHierSpar(), input$inPlotHierSparNclust, getDataIDs()), file = file, row.names = FALSE)
    }
  )
  
  # Function instead of reactive as per:
  # http://stackoverflow.com/questions/26764481/downloading-png-from-shiny-r
  # This function is used to plot and to downoad a pdf
  plotHierSpar <- function() {
    cat(file = stderr(), 'plotHierSpar \n')
    
    loc.dm = dataMod()
    if (is.null(loc.dm))
      return(NULL)
    
    loc.sphc <- userFitHierSpar()
    if (is.null(loc.sphc))
      return(NULL)
    
    loc.dend <- as.dendrogram(loc.sphc$hc)
    loc.dend <-
      color_branches(loc.dend, k = input$inPlotHierSparNclust)
    
    if (input$inPlotHierSparRevPalette)
      my_palette <-
      rev(colorRampPalette(brewer.pal(9, input$selectPlotHierSparPalette))(n = 99))
    else
      my_palette <-
      colorRampPalette(brewer.pal(9, input$selectPlotHierSparPalette))(n = 99)
    
    
    col_labels <- get_leaves_branches_col(loc.dend)
    col_labels <- col_labels[order(order.dendrogram(loc.dend))]
    
    if (input$selectPlotHierSparDend) {
      assign("var.tmp.1", loc.dend)
      var.tmp.2 = "row"
    } else {
      assign("var.tmp.1", FALSE)
      var.tmp.2 = "none"
    }
    
    loc.colnames = paste0(ifelse(loc.sphc$ws == 0, "",
                                 ifelse(
                                   loc.sphc$ws <= 0.1,
                                   "* ",
                                   ifelse(loc.sphc$ws <= 0.5, "** ", "*** ")
                                 )), colnames(loc.dm))
    
    loc.colcol   = ifelse(loc.sphc$ws == 0,
                          "black",
                          ifelse(
                            loc.sphc$ws <= 0.1,
                            "blue",
                            ifelse(loc.sphc$ws <= 0.5, "green", "red")
                          ))
    
    
    loc.p = heatmap.2(
      loc.dm,
      Colv = "NA",
      Rowv = var.tmp.1,
      srtCol = 90,
      dendrogram = var.tmp.2,
      trace = "none",
      key = input$selectPlotHierSparKey,
      margins = c(
        input$inPlotHierSparMarginX,
        input$inPlotHierSparMarginY
      ),
      col = my_palette,
      na.col = grey(input$inPlotHierSparNAcolor),
      denscol = "black",
      density.info = "density",
      RowSideColors = col_labels,
      colRow = col_labels,
      colCol = loc.colcol,
      labCol = loc.colnames,
      sepcolor = if (input$inDispGrid) grey(input$inPlotHierSparGridColor) else NULL,
      colsep = if (input$inDispGrid) 1:ncol(loc.dm) else NULL,
      rowsep = if (input$inDispGrid) 1:nrow(loc.dm) else NULL,
      cexRow = input$inPlotHierSparFontX,
      cexCol = input$inPlotHierSparFontY,
      main = paste(
        "Distance measure: ",
        s.cl.spar.dist[as.numeric(input$selectPlotHierSparDist)],
        "\nLinkage method: ",
        s.cl.spar.linkage[as.numeric(input$selectPlotHierSparLinkage)]
      )
    )
    
    return(loc.p)
  }
  
  # Sparse Hierarchical - display plot
  output$outPlotHierSpar <- renderPlot({
    plotHierSpar()
  })
  
  createFnameHeatMap = reactive({
    
    paste0('clust_hierchSparse_',  
           s.cl.spar.dist[as.numeric(input$selectPlotHierSparDist)],
           "_",
           s.cl.spar.linkage[as.numeric(input$selectPlotHierSparLinkage)], '.png')
    
  })
  
  # Sparse Hierarchical - download png
  callModule(downPlot, "downPlotHierSparPNG", createFnameHeatMap, plotHierSpar)
  

  # Sparse Hierarchical clustering (sparcl) interactive version
  output$plotHierSparInt <- renderD3heatmap({
    cat(file = stderr(), 'plotHierSparInt \n')
    
    loc.dm = dataMod()
    if (is.null(loc.dm))
      return(NULL)
    
    loc.sphc <- userFitHierSpar()
    if (is.null(loc.sphc))
      return(NULL)
    
    loc.dend <- as.dendrogram(loc.sphc$hc)
    loc.dend <-
      color_branches(loc.dend, k = input$inPlotHierSparNclust)
    
    if (input$inPlotHierSparRevPalette)
      my_palette <-
      rev(colorRampPalette(brewer.pal(9, input$selectPlotHierSparPalette))(n = 99))
    else
      my_palette <-
      colorRampPalette(brewer.pal(9, input$selectPlotHierSparPalette))(n = 99)
    
    
    col_labels <- get_leaves_branches_col(loc.dend)
    col_labels <- col_labels[order(order.dendrogram(loc.dend))]
    
    if (input$selectPlotHierSparDend) {
      assign("var.tmp.1", loc.dend)
      var.tmp.2 = "row"
    } else {
      assign("var.tmp.1", FALSE)
      var.tmp.2 = "none"
    }
    
    loc.colnames = paste0(colnames(loc.dm), ifelse(loc.sphc$ws == 0, "",
                                                   ifelse(
                                                     loc.sphc$ws <= 0.1,
                                                     " *",
                                                     ifelse(loc.sphc$ws <= 0.5, " **", " ***")
                                                   )))
    
    d3heatmap(
      loc.dm,
      Rowv = var.tmp.1,
      dendrogram = var.tmp.2,
      trace = "none",
      revC = FALSE,
      na.rm = FALSE,
      margins = c(
        input$inPlotHierSparMarginX * 10,
        input$inPlotHierSparMarginY * 10
      ),
      colors = my_palette,
      na.col = grey(input$inPlotHierSparNAcolor),
      cexRow = input$inPlotHierSparFontY * 0.5,
      cexCol = input$inPlotHierSparFontX * 0.5,
      xaxis_height = input$inPlotHierSparMarginX * 10,
      yaxis_width = input$inPlotHierSparMarginY * 10,
      show_grid = TRUE,
      #labRow = rownames(dm.t),
      labCol = loc.colnames
    )
  })

  
  output$inGridColorUI <- renderUI({
    ns <- session$ns
    
    if(input$inDispGrid) {
      sliderInput(
        ns('inPlotHierSparGridColor'),
        'Shade of grey for grid lines (0 - black, 1 - white)',
        min = 0,
        max = 1,
        value = 0.6,
        step = .1,
        ticks = TRUE)
    }
  })
  
  # Sparse Hierarchical - choose to display regulat heatmap.2 or d3heatmap (interactive)
  output$plotHierSparInt_ui <- renderUI({
    ns <- session$ns
    
    if (input$inPlotHierSparInteractive)
      d3heatmapOutput(ns("plotHierSparInt"), height = paste0(input$inPlotHeight, "px"), width = paste0(input$inPlotWidth, "px"))
    else
      plotOutput(ns('outPlotHierSpar'), height = paste0(input$inPlotHeight, "px"), width = paste0(input$inPlotWidth, "px"))
  })
  
}