#
# Free-Clust: Shiny app for clustering data
# Author: Maciej Dobrzynski
#
# This module is a tab for hierarchical clustering (base R hclust + dist)
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


require(gplots) # heatmap.2
require(dendextend) # color_branches
require(RColorBrewer) # brewer.pal
require(d3heatmap) # interactive heatmap
require(shinyBS) # for tooltips
require(shinycssloaders) # for loader animations

helpText.clHier = c(alertNAsPresentClDTW = paste0("NAs (still) present in the dataset. DTW cannot calculate the distance. "),
                    alertNAsPresentCl = paste0("NAs (still) present in the dataset, caution recommended."),
                    alertNAsPresentDist = "Calculation of the distance matrix yielded NAs. Dataset might be too sparse",
                    alLearnMore = paste0("<p><a href=\"https://en.wikipedia.org/wiki/Hierarchical_clustering\" target=\"_blank\" title=\"External link\">Agglomerative hierarchical clustering</a> ",
                                         "initially assumes that all data points are forming their own clusters. It then grows a clustering dendrogram using two inputs:<p>",
                                         "A <b>dissimilarity matrix</b> between sample pairs ",
                                         "is calculated with one of the metrics, such as ",
                                         "Euclidean (<a href=\"https://en.wikipedia.org/wiki/Euclidean_distance\" target=\"_blank\" title=\"External link\">L2 norm</a>), ",
                                         "Manhattan (<a href=\"https://en.wikipedia.org/wiki/Taxicab_geometry\" target=\"_blank\" title=\"External link\">L1 norm</a>), or ",
                                         "<a href=\"https://en.wikipedia.org/wiki/Dynamic_time_warping\" target=\"_blank\" title=\"External link\">Dynamic Time Warping</a> (DTW). ",
                                         "Instead of comparing features one by one, DTW tries to align and match their shapes. ",
                                         "This makes DTW a good quantification of similarity when the order of features matters, as is the case in time series.</p>",
                                         "<p>In the second step, clusters are successively built and merged together. The distance between the newly formed clusters is determined by the <b>linkage criterion</b> ",
                                         "using one of <a href=\"https://en.wikipedia.org/wiki/Hierarchical_clustering\" target=\"_blank\" title=\"External link\">linkage methods</a>.</p>"),
                    downClAss = "Download a CSV with cluster assignments to time series ID",
                    downDend = "Download an RDS file with dendrogram object. Read later with readRDS() function.")

# UI ----
clustHierUI <- function(id, label = "Hierarchical CLustering") {
  ns <- NS(id)
  
  tagList(
    h4('Hierarchical clustering'),
    p("Standard approach using R's ",
      a("dist", 
        href = "https://stat.ethz.ch/R-manual/R-devel/library/stats/html/dist.html",
        title ="External link",
        target = "_blank"),
      " and ",
      a("hclust", 
        href = "https://stat.ethz.ch/R-manual/R-devel/library/stats/html/hclust.html",
        title = "External link", 
        target = "_blank"),
      " functions. ",
      actionLink(ns("alLearnMore"), "Learn more")
    ),
    br(),
    
    fluidRow(
      column(
        3,
        selectInput(
          ns("selectDist"),
          label = ("Dissimilarity measure"),
          choices = list("Euclidean" = "euclidean",
                         "Manhattan" = "manhattan",
                         "Maximum"   = "maximum",
                         "Canberra"  = "canberra",
                         "DTW" = "DTW"),
          selected = 1
        ),
        bsAlert("alertAnchorClHierNAsPresent"),
        selectInput(
          ns("selectLinkage"),
          label = ("Linkage method"),
          choices = list(
            "Average"  = "average",
            "Complete" = "complete",
            "Single"   = "single",
            "Centroid" = "centroid",
            "Ward"     = "ward.D",
            "Ward D2"  = "ward.D2",
            "McQuitty" = "mcquitty"),
          selected = 1
        ),
        checkboxInput(ns('selectDend'), 
                      'Plot dendrogram and re-order samples', 
                      TRUE),
        sliderInput(
          ns('slNclust'),
          'Number of dendrogram branches to cut',
          min = 1,
          max = 10,
          value = 1,
          step = 1,
          ticks = TRUE,
          round = TRUE
        ),
        
      ),
      
      column(
        3,
        selectInput(
          ns("selectPalette"),
          label = "Heatmap\'s colour palette:",
          choices = l.col.pal,
          selected = 'Spectral'
        ),
        selectInput(
          ns("selectPlotHierPaletteDend"),
          label = "Dendrogram\'s colour palette",
          choices = l.col.pal.dend,
          selected = 'Color Blind'
        ),
        checkboxInput(ns('inRevPalette'), 
                      'Reverse colour palette', 
                      TRUE),
        checkboxInput(ns('selectKey'), 
                      'Plot colour key', 
                      TRUE)
      ),
      column(
        3,
        checkboxInput(ns('inDispGrid'), 
                      'Display grid lines', 
                      FALSE),
        uiOutput(ns('inGridColorUI')),
        sliderInput(
          ns('inNAcolor'),
          'Shade of grey for NA values',
          min = 0,
          max = 1,
          value = 0.8,
          step = .1,
          ticks = TRUE
        )
      )
    ),
    
    br(),
    checkboxInput(ns('chBplotStyle'),
                  'Adjust plot appearance',
                  FALSE),
    conditionalPanel(
      condition = "input.chBplotStyle",
      ns = ns,
      fluidRow(
        column(2,
               numericInput(
                 ns('inMarginX'),
                 'Bottom margin',
                 10,
                 min = 1,
                 width = 100
               )
        ),
        column(2,
               numericInput(
                 ns('inMarginY'),
                 'Right margin',
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
               downloadButton(ns('downClAss'), 'Cluster assignments'),
               bsTooltip(ns("downClAss"),
                         helpText.clHier[["downClAss"]],
                         placement = "top",
                         trigger = "hover",
                         options = NULL)
               ),
        column(4,
               downloadButton(ns('downDend'), 'Dendrogram object'),
               bsTooltip(ns("downDend"),
                         helpText.clHier[["downDend"]],
                         placement = "top",
                         trigger = "hover",
                         options = NULL)
               )
      ),
      
      downPlotUI(ns('downPlotHierPNG'), "")
    ),
    
    checkboxInput(ns('plotInt'), 
                  'Interactive Plot',
                  value = FALSE),
    uiOutput(ns("plotUI"))
    
  )
}

# SERVER ----
clustHier <- function(input, output, session, dataMod) {
  
  ns <- session$ns
  
  # Return the number of clusters from the slider 
  # and delay by a constant in milliseconds defined in auxfunc.R
  returnNclust = reactive({
    return(input$slNclust)
  }) %>% debounce(MILLIS)
  
  # calculate distance matrix for further clustering
  # samples arranged in rows with columns corresponding to measurements/features
  calcDist <- reactive({
    cat(file = stdout(), 'calcDist \n')
    
    locDM = dataMod()
    
    if (is.null(locDM)) {
      return(NULL)
    }
    
    # Throw some warnings if NAs present in the dataset.
    # DTW cannot compute distance when NA's are preset.
    # Other distance measures can be calculated but caution is required with interpretation.
    # NAs in the wide format can result from explicit NAs in the measurment column or
    # from missing rows that cause NAs to appear when convertinf from long to wide (dcast)
    if(sum(is.na(locDM)) > 0) {
      if (input$selectDist == "DTW") {
        createAlert(session, "alertAnchorClHierNAsPresent", "alertNAsPresentClDTW", title = "Error",
                    content = helpText.clHier[["alertNAsPresentClDTW"]], 
                    append = FALSE,
                    style = "danger")
        closeAlert(session, 'alertNAsPresentCl')
        
        return(NULL)
        
      } else {
        createAlert(session, "alertAnchorClHierNAsPresent", "alertNAsPresentCl", title = "Warning",
                    content = helpText.clHier[["alertNAsPresentCl"]], 
                    append = FALSE, 
                    style = "warning")
        closeAlert(session, 'alertNAsPresentClDTW')
      }
    } else {
      closeAlert(session, 'alertNAsPresentClDTW')
      closeAlert(session, 'alertNAsPresentCl')
    }
    
    
    #pr_DB$set_entry(FUN = fastDTW, names = c("fastDTW"))
    cl.dist = proxy::dist(locDM, 
                          method = input$selectDist)
    
    return(cl.dist)
  })
  
  calcDend <- reactive({
    cat(file = stdout(), 'calcDend \n')
    
    loc.dist = calcDist()
    
    if (is.null(loc.dist)) {
      return(NULL)
    } else if (sum(is.na(loc.dist)) > 0) {
      createAlert(session, "alertAnchorClHierNAsPresent", "alertNAsPresentDist", title = "Error",
                  content = helpText.clHier[["alertNAsPresentDist"]], 
                  append = FALSE, 
                  style = "danger")
      
      return(NULL)
    } else {
      closeAlert(session, "alertNAsPresentDist")
    }
    
    loc.cl.hc = hclust(loc.dist, method = input$selectLinkage)
    
    # number of clusters at which dendrogram is cut
    loc.k = returnNclust()
    
    # make a palette with the amount of colours equal to the number of clusters
    loc.col = ggthemes::tableau_color_pal(input$selectPlotHierPaletteDend)(n = loc.k)
    
    loc.dend <- as.dendrogram(loc.cl.hc)
    loc.dend <- color_branches(loc.dend, 
                               col = loc.col,
                               k = loc.k)
    
    return(loc.dend)
  })
  
  
  # download a list of IDs with cluster assignments
  output$downClAss <- downloadHandler(
    filename = function() {
      paste0('clust_hier_data_',
             input$selectDist,
             '_',
             input$selectLinkage, 
             '.csv')
    },
    
    content = function(file) {
      fwrite(x = myGetDataCl(calcDend(), 
                           input$slNclust), 
             file = file, 
             row.names = FALSE)
    }
  )
  
  # download an RDS file with dendrogram objet
  output$downDend <- downloadHandler(
    filename = function() {
      paste0('clust_hier_dend_',
             input$selectDist,
             '_',
             input$selectLinkage, '.rds')
    },
    
    content = function(file) {
      saveRDS(object = calcDend(), file = file)
    }
  )
  
  
  
  # Function instead of reactive as per:
  # http://stackoverflow.com/questions/26764481/downloading-png-from-shiny-r
  # This function is used to plot and to downoad a pdf
  
  plotHier <- function() {
    cat(file = stdout(), 'plotHier \n')
    
    locDM = dataMod()
    loc.dend <- calcDend()
    
    validate(
      need(!is.null(locDM), "Nothing to plot. Load data first!"),
      need(!is.null(loc.dend), "Did not create dendrogram")
    )
    
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
    
    heatmap.2(
      locDM,
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
      colsep = if (input$inDispGrid) 1:ncol(locDM) else NULL,
      rowsep = if (input$inDispGrid) 1:nrow(locDM) else NULL,
      cexRow = input$inFontX,
      cexCol = input$inFontY,
      main = paste(
        "Distance measure: ",
        input$selectDist,
        "\nLinkage method: ",
        input$selectLinkage
      )
    )
  }
  
  # Hierarchical - display plot
  output$outPlotHier <- renderPlot({
    plotHier()
  })
  
  createFnameHeatMap = reactive({
    
    paste0('clust_hier_',
           input$selectDist,
           '_',
           input$selectLinkage,
           '.png')
  })
  
  # Hierarchical - download png
  callModule(downPlot, "downPlotHierPNG", createFnameHeatMap, plotHier)
  
  # Hierarchical clustering - interactive version
  output$outPlotInt <- renderD3heatmap({
    cat(file = stdout(), 'Int \n')
    
    locDM = dataMod()
    loc.dend <- calcDend()
    
    validate(
      need(!is.null(locDM), "Nothing to plot. Load data first!"),
      need(!is.null(loc.dend), "Did not create dendrogram")
    )
    
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
      locDM,
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
        'Shade of grey for grid lines',
        min = 0,
        max = 1,
        value = 0.6,
        step = .1,
        ticks = TRUE)
    }
  })
  
  # Hierarchical - choose to display regular heatmap.2 or d3heatmap (interactive)
  output$plotUI <- renderUI({
    ns <- session$ns
    if (input$plotInt)
        withSpinner(d3heatmapOutput(ns("outPlotInt"), 
                              height = paste0(input$inPlotHeight, "px"), 
                              width = paste0(input$inPlotWidth, "px")))
    else
        withSpinner(plotOutput(ns('outPlotHier'), 
                               height = paste0(input$inPlotHeight, "px"), 
                               width = paste0(input$inPlotWidth, "px")))
  })
  
  # Pop-overs ----
  addPopover(session, 
             ns("alLearnMore"),
             title = "Hierarchical clustering",
             content = helpText.clHier[["alLearnMore"]],
             trigger = "click")
  
  
}