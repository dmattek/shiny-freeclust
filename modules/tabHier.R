# RShiny module for performing hierarchical clustering
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

s.cl.linkage = c("ward.D",
                 "ward.D2",
                 "single",
                 "complete",
                 "average",
                 "mcquitty",
                 "centroid")

s.cl.dist = c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")

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
clustHierUI <- function(id, label = "Hierarchical CLustering") {
  ns <- NS(id)
  
  tagList(
    h4(
      "Hierarchical clustering using standard",
      a("hclust", href = "https://stat.ethz.ch/R-manual/R-devel/library/stats/html/hclust.html")
    ),
    br(),
    
    fluidRow(
      column(
        6,
        selectInput(
          ns("selectDist"),
          label = ("Select distance method:"),
          choices = list(
            "Euclidean" = 1,
            "Maximum" = 2,
            "Manhattan" = 3,
            "Canberra" = 4,
            "Binary" = 5,
            "Minkowski" = 6
          ),
          selected = 1
        ),
        selectInput(
          ns("selectLinkage"),
          label = ("Select linkage method:"),
          choices = list(
            "Ward" = 1,
            "Ward D2" = 2,
            "Single" = 3,
            "Complete" = 4,
            "Average" = 5,
            "McQuitty" = 6,
            "Centroid" = 7
          ),
          selected = 1
        ),
        checkboxInput(ns('selectDend'), 
                      'Plot dendrogram and re-order samples', 
                      TRUE),
        sliderInput(
          ns('inNclust'),
          '#dendrogram branches to colour',
          min = 1,
          max = 10,
          value = 1,
          step = 1,
          ticks = TRUE,
          round = TRUE
        ),
        checkboxInput(ns('selectKey'), 
                      'Plot colour key', 
                      TRUE),
        downloadButton(ns('downCellCl'), 'Download CSV with cluster associations')
      ),
      column(
        6,
        selectInput(
          ns("selectPalette"),
          label = "Select colour palette:",
          choices = l.col.pal,
          selected = 'Spectral'
        ),
        checkboxInput(ns('inRevPalette'), 
                      'Reverse colour palette', 
                      TRUE),
        sliderInput(
          ns('inNAcolor'),
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
    
    br(),
    fluidRow(
      column(2,
             numericInput(
               ns('inMarginX'),
               'Margin below x-axis',
               10,
               min = 1,
               width = 100
             )
      ),
      column(2,
             numericInput(
               ns('inMarginY'),
               'Margin right of y-axis',
               10,
               min = 1,
               width = 100
             )
      ),
      column(2,
             numericInput(
               ns('inFontX'),
               'Font size row labels',
               1,
               min = 0,
               width = 100,
               step = 0.1
             )
      ),
      column(2,
        numericInput(
          ns('inFontY'),
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
    downPlotUI(ns('downPlotHier'), "Download PDF"),
    
    br(),
    checkboxInput(ns('plotInt'), 
                  'Interactive Plot?',
                  value = FALSE),
    uiOutput(ns("plotInt_ui"))
    
  )
}

# SERVER
clustHier <- function(input, output, session, dataMod) {
  
  userFitDendHier <- reactive({
    cat(file = stderr(), 'userFitDendHier \n')
    
    loc.dm = dataMod()
    if (is.null(loc.dm))
      return(NULL)
    
    loc.cl.dist = dist(loc.dm,  method = s.cl.dist[as.numeric(input$selectDist)])
    loc.cl.hc = hclust(loc.cl.dist, method = s.cl.linkage[as.numeric(input$selectLinkage)])
    
    loc.dend <- as.dendrogram(loc.cl.hc)
    loc.dend <- color_branches(loc.dend, k = input$inNclust)
    
    return(loc.dend)
  })
  
  
  # download a list of IDs with cluster assignments
  output$downCellCl <- downloadHandler(
    filename = function() {
      paste0('clust_hierch_data_',
             s.cl.dist[as.numeric(input$selectDist)],
             '_',
             s.cl.linkage[as.numeric(input$selectLinkage)], '.csv')
    },
    
    content = function(file) {
      write.csv(x = getDataCl(userFitDendHier(), input$inNclust), file = file, row.names = FALSE)
    }
  )
  
  
  
  # Function instead of reactive as per:
  # http://stackoverflow.com/questions/26764481/downloading-png-from-shiny-r
  # This function is used to plot and to downoad a pdf
  
  plotHier <- function() {
    cat(file = stderr(), 'plotHier \n')
    
    in.dm = dataMod()
    if (is.null(in.dm))
      return(NULL)
    
    in.dend <- userFitDendHier()
    if (is.null(in.dend))
      return(NULL)
    
    if (input$inRevPalette)
      my_palette <-
      rev(colorRampPalette(brewer.pal(9, input$selectPalette))(n = 99))
    else
      my_palette <-
      colorRampPalette(brewer.pal(9, input$selectPalette))(n = 99)
    
    
    col_labels <- get_leaves_branches_col(in.dend)
    col_labels <- col_labels[order(order.dendrogram(in.dend))]
    
    if (input$selectDend) {
      assign("var.tmp.1", in.dend)
      var.tmp.2 = "row"
    } else {
      assign("var.tmp.1", FALSE)
      var.tmp.2 = "none"
    }
    
    heatmap.2(
      in.dm,
      Colv = "NA",
      Rowv = var.tmp.1,
      srtCol = 90,
      dendrogram = var.tmp.2,
      trace = "none",
      key = input$selectKey,
      margins = c(input$inMarginX, input$inMarginY),
      col = my_palette,
      na.col = grey(input$inNAcolor),
      denscol = "black",
      density.info = "density",
      RowSideColors = col_labels,
      colRow = col_labels,
      sepcolor = if (input$inDispGrid) grey(input$inGridColor) else NULL,
      colsep = if (input$inDispGrid) 1:ncol(in.dm) else NULL,
      rowsep = if (input$inDispGrid) 1:nrow(in.dm) else NULL,
      cexRow = input$inFontX,
      cexCol = input$inFontY,
      main = paste(
        "Distance measure: ",
        s.cl.dist[as.numeric(input$selectDist)],
        "\nLinkage method: ",
        s.cl.linkage[as.numeric(input$selectLinkage)]
      )
    )
  }
  
  # Hierarchical - display plot
  output$outPlotHier <- renderPlot({
    plotHier()
  })
  
  # Hierarchical - download pdf
  callModule(downPlot, "downPlotHier",       paste0('clust_hierch_',
                                                    s.cl.dist[as.numeric(input$selectDist)],
                                                    '_',
                                                    s.cl.linkage[as.numeric(input$selectLinkage)], '.pdf'), plotHier)
  
  # Hierarchical clustering - interactive version
  output$outPlotInt <- renderD3heatmap({
    cat(file = stderr(), 'Int \n')
    
    loc.dm = dataMod()
    if (is.null(loc.dm))
      return(NULL)
    
    loc.dend <- userFitDendHier()
    if (is.null(loc.dend))
      return(NULL)
    
    if (input$inRevPalette)
      my_palette <-
      rev(colorRampPalette(brewer.pal(9, input$selectPalette))(n = 99))
    else
      my_palette <-
      colorRampPalette(brewer.pal(9, input$selectPalette))(n = 99)
    
    
    col_labels <- get_leaves_branches_col(loc.dend)
    col_labels <- col_labels[order(order.dendrogram(loc.dend))]
    
    if (input$selectDend) {
      assign("var.tmp.1", loc.dend)
      var.tmp.2 = "row"
    } else {
      assign("var.tmp.1", FALSE)
      var.tmp.2 = "none"
    }
    
    d3heatmap(
      loc.dm,
      Rowv = var.tmp.1,
      dendrogram = var.tmp.2,
      trace = "none",
      revC = FALSE,
      na.rm = FALSE,
      margins = c(input$inMarginX * 10, input$inMarginY * 10),
      colors = my_palette,
      na.col = grey(input$inNAcolor),
      cexRow = input$inFontY * 0.5,
      cexCol = input$inFontX * 0.5,
      xaxis_height = input$inMarginX * 10,
      yaxis_width = input$inMarginY * 10,
      show_grid = TRUE
    )
  })
 
  output$inGridColorUI <- renderUI({
    ns <- session$ns
    
    if(input$inDispGrid) {
      sliderInput(
        ns('inGridColor'),
        'Shade of grey for grid lines (0 - black, 1 - white)',
        min = 0,
        max = 1,
        value = 0.6,
        step = .1,
        ticks = TRUE)
    }
  })
   
  # Hierarchical - choose to display regular heatmap.2 or d3heatmap (interactive)
  output$plotInt_ui <- renderUI({
    ns <- session$ns
    if (input$plotInt)
      tagList(d3heatmapOutput(ns("outPlotInt"), height = paste0(input$inPlotHeight, "px"), width = paste0(input$inPlotWidth, "px")))
    else
      tagList(plotOutput(ns('outPlotHier'), height = paste0(input$inPlotHeight, "px"), width = paste0(input$inPlotWidth, "px")))
  })
  
 
}