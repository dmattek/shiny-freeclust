#
# Free-Clust: Shiny app for clustering data
# Author: Maciej Dobrzynski
#
# RShiny module for performing sparse hierarchical clustering using sparcl
# Use:
# in ui.R
# tabPanel(
#  'Sparse Hier.',
#  clustHierUI('TabClustHierSpar'))
#
# in server.R
# callModule(clustHierSpar, 'TabClustHierSpar', dataMod)
# where dataMod is the output from a reactive function 
# that returns a dataset in wide format ready for clustering


require(gplots) # heatmap.2
require(dendextend) # color_branches
require(RColorBrewer) # brewer.pal
require(d3heatmap) # interactive heatmap
require(sparcl) # sparse hierarchical and k-means
require(shinyBS) # for tooltips
require(shinycssloaders) # for loader animations


helpText.clHierSpar = c(alImportance = paste0("<p>Weight factors (WF) calculated during clustering ",
                                              "reflect the importance of data points in the clustering. ",
                                              "The following labels are used to indicate the importance:",
                                              "<li>Black - time point not taken into account</li>",
                                              "<li><p>* - low, WF∈(0, 0.1]</p></li>",
                                              "<li><p>** - medium, WF∈(0.1, 0.5]</p></li>",
                                              "<li><p>*** - high, WF∈(0.5, 1.0]</p></li>",
                                              "</p><p>Witten and Tibshirani (2010): ",
                                              "<i>A framework for feature selection in clustering</i>; ",
                                              "Journal of the American Statistical Association 105(490): 713-726.</p>"),
                        downClAss = "Download a CSV with cluster assignments to time series ID",
                        downDend = "Download an RDS file with dendrogram object. Read later with readRDS() function.")


# UI ----
clustHierSparUI <- function(id, label = "Sparse Hierarchical CLustering") {
  ns <- NS(id)
  
  tagList(
    h4(
      "Sparse hierarchical clustering using ",
      a("sparcl", 
        href = "https://cran.r-project.org/web/packages/sparcl/",
        title="External link",
        target = "_blank")
    ),
    p("Columns in the heatmap labeled according to their ",
      actionLink(ns("alImportance"), "importance.")),
    br(),
    fluidRow(
      column(
        3,
        selectInput(
          ns("selectPlotHierSparDist"),
          label = ("Dissimilarity measure"),
          choices = list("Euclidean" = "squared.distance",
                         "Manhattan" = "absolute.value"),
          selected = 1
        ),
        selectInput(
          ns("selectPlotHierSparLinkage"),
          label = ("Linkage method"),
          choices = list(
            "Average"  = "average",
            "Complete" = "complete",
            "Single"   = "single",
            "Centroid" = "centroid"
          ),
          selected = 1
        ),
        checkboxInput(ns('selectPlotHierSparDend'), 'Plot dendrogram and re-order samples', TRUE),
        sliderInput(
          ns('slNclust'),
          'Number of dendrogram branches to cut',
          min = 1,
          max = 10,
          value = 1,
          step = 1,
          ticks = TRUE,
          round = TRUE
        )
      ),
      
      column(
        3,
        selectInput(
          ns("selectPlotHierSparPalette"),
          label = "Heatmap\'s colour palette:",
          choices = l.col.pal,
          selected = 'Spectral'
        ),
        selectInput(
          ns("selectPlotHierSparPaletteDend"),
          label = "Dendrogram\'s colour palette",
          choices = l.col.pal.dend,
          selected = 'Color Blind'
        ),
        checkboxInput(ns('inPlotHierSparRevPalette'), 'Reverse colour palette', TRUE),
        checkboxInput(ns('selectPlotHierSparKey'), 'Plot colour key', TRUE),
        checkboxInput(ns('inHierSparAdv'),
                      'Advanced options',
                      FALSE),
        
        # Only show this panel if inHierSparAdv == TRUE
        conditionalPanel(
          condition = "input.inHierSparAdv == 1",
          ns = ns, 
          sliderInput(
            ns('inPlotHierSparNperms'),
            'Number of permutations',
            min = 1,
            max = 20,
            value = 10,
            step = 1,
            ticks = TRUE
          ),
          sliderInput(
            ns('inPlotHierSparNiter'),
            'Number of iterations',
            min = 1,
            max = 50,
            value = 15,
            step = 1,
            ticks = TRUE
          )
        ),
        
      ),
      column(3,
             checkboxInput(ns('inDispGrid'), 
                           'Display grid lines', 
                           FALSE),
             uiOutput(ns('inGridColorUI')),
             sliderInput(
               ns('inPlotHierSparNAcolor'),
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
        column(
          2,
          numericInput(
            ns('inPlotHierSparMarginX'),
            'Bottom margin',
            10,
            min = 1,
            width = 100
          )
        ),
        column(
          2,
          numericInput(
            ns('inPlotHierSparMarginY'),
            'Right margin',
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
               
               downloadButton(ns('downClAssSpar'), 'Cluster assignments'),
               bsTooltip(ns("downClAssSpar"),
                         helpText.clHierSpar[["downClAss"]],
                         placement = "top",
                         trigger = "hover",
                         options = NULL)
        ),
        
        column(4,
               
               downloadButton(ns('downDendSpar'), 'Dendrogram object'),
               bsTooltip(ns("downDendSpar"),
                         helpText.clHierSpar[["downDend"]],
                         placement = "top",
                         trigger = "hover",
                         options = NULL))
      ),
      downPlotUI(ns('downPlotHierSparPNG'), "")
    ),
    
    checkboxInput(ns('inPlotHierSparInteractive'), 
                  'Interactive Plot',  
                  value = FALSE),
    uiOutput(ns("plotUI"))
  )
}

# SERVER ----
clustHierSpar <- function(input, output, session, dataMod) {
  ns = session$ns
  
  
  # Return the number of clusters from the slider 
  # and delay by a constant in milliseconds defined in auxfunc.R
  returnNclust = reactive({
    return(input$slNclust)
  }) %>% debounce(MILLIS)
  
  calcHierSpar <- reactive({
    cat(file = stdout(), 'calcHierSpar \n')
    
    locDM = dataMod()
    if (is.null(locDM))
      return(NULL)
    
    perm.out <- HierarchicalSparseCluster.permute(
      locDM,
      wbounds = NULL,
      nperms = input$inPlotHierSparNperms,
      dissimilarity = input$selectPlotHierSparDist
    )
    
    sparsehc <- HierarchicalSparseCluster(
      dists = perm.out$dists,
      wbound = perm.out$bestw,
      niter = input$inPlotHierSparNiter,
      method = input$selectPlotHierSparLinkage,
      dissimilarity = input$selectPlotHierSparDist
    )
    return(sparsehc)
  })

  
  calcDend <- reactive({
    cat(file = stdout(), 'calcDend \n')
    
    loc.hc = calcHierSpar()
    if (is.null(loc.hc))
      return(NULL)
    
    # number of clusters at which dendrogram is cut
    loc.k = returnNclust()
    
    # make a palette with the amount of colours equal to the number of clusters
    loc.col = ggthemes::tableau_color_pal(input$selectPlotHierSparPaletteDend)(n = loc.k)
    
    
    dend <- as.dendrogram(loc.hc$hc)
    dend <- color_branches(dend, 
                           col = loc.col,
                           k = loc.k)
    
    return(dend)
  })
  
  # return all IDs (created in dataMod)
  # used when saving cluster associations in sparse hierarchical
  # sparsehc doesn't return original rownames after clustering
  getDataIDs <- reactive({
    cat(file = stdout(), 'getDataIDs\n')
    loc.m = dataMod()
    
    if (is.null(loc.m))
      return(NULL)
    else
      return(rownames(loc.m))
  })
  
  # download a list of IDs with cluster assignments
  output$downClAssSpar <- downloadHandler(
    filename = function() {
      paste0('clust_hierSpar_data_',
             input$selectPlotHierSparDist,
             '_',
             input$selectPlotHierSparLinkage, '.csv')
    },
    
    content = function(file) {
      fwrite(x = myGetDataClSpar(calcDend(), 
                               input$slNclust, 
                               getDataIDs()), 
             file = file, 
             row.names = FALSE)
    }
  )
  
  # download an RDS file with dendrogram objet
  output$downDendSpar <- downloadHandler(
    filename = function() {
      paste0('clust_hierSpar_dend_',
             input$selectPlotHierSparDist,
             '_',
             input$selectPlotHierSparLinkage, '.rds')
    },
    
    content = function(file) {
      saveRDS(object = calcDend(), file = file)
    }
  )
  
  
  # Function instead of reactive as per:
  # http://stackoverflow.com/questions/26764481/downloading-png-from-shiny-r
  # This function is used to plot and to downoad a pdf
  plotHierSpar <- function() {
    cat(file = stdout(), 'plotHierSpar \n')
    
    locDM = dataMod()
    loc.sphc <- calcHierSpar()
    loc.dend <- calcDend()
    
    validate(
      need(!is.null(locDM), "Nothing to plot. Load data first!"),
      need(!is.null(loc.sphc), "Did not cluster"),
      need(!is.null(loc.dend), "Did not create dendrogram")
    )
    
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
                                 )), colnames(locDM))
    
    loc.colcol   = ifelse(loc.sphc$ws == 0,
                          "black",
                          ifelse(
                            loc.sphc$ws <= 0.1,
                            "blue",
                            ifelse(loc.sphc$ws <= 0.5, "green", "red")
                          ))
    
    
    loc.p = heatmap.2(
      locDM,
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
      colsep = if (input$inDispGrid) 1:ncol(locDM) else NULL,
      rowsep = if (input$inDispGrid) 1:nrow(locDM) else NULL,
      cexRow = input$inPlotHierSparFontX,
      cexCol = input$inPlotHierSparFontY,
      main = paste(
        "Distance measure: ",
        input$selectPlotHierSparDist,
        "\nLinkage method: ",
        input$selectPlotHierSparLinkage
      )
    )
    
    return(loc.p)
  }
  
  # Sparse Hierarchical - display plot
  output$outPlotHierSpar <- renderPlot({
    plotHierSpar()
  })
  
  createFnameHeatMap = reactive({
    
    paste0('clust_hierSpar_',  
           input$selectPlotHierSparDist,
           "_",
           input$selectPlotHierSparLinkage, 
           '.png')
    
  })
  
  # Sparse Hierarchical - download png
  callModule(downPlot, "downPlotHierSparPNG", createFnameHeatMap, plotHierSpar)
  

  # Sparse Hierarchical clustering (sparcl) interactive version
  output$plotHierSparInt <- renderD3heatmap({
    cat(file = stdout(), 'plotHierSparInt \n')
    
    locDM = dataMod()
    loc.sphc <- calcHierSpar()
    loc.dend <- calcDend()
    
    validate(
      need(!is.null(locDM), "Nothing to plot. Load data first!"),
      need(!is.null(loc.sphc), "Did not cluster"),
      need(!is.null(loc.dend), "Did not create dendrogram")
    )
    
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
    
    loc.colnames = paste0(colnames(locDM), ifelse(loc.sphc$ws == 0, "",
                                                   ifelse(
                                                     loc.sphc$ws <= 0.1,
                                                     " *",
                                                     ifelse(loc.sphc$ws <= 0.5, " **", " ***")
                                                   )))
    
    d3heatmap(
      locDM,
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
        'Shade of grey for grid lines',
        min = 0,
        max = 1,
        value = 0.6,
        step = .1,
        ticks = TRUE)
    }
  })
  
  # Sparse Hierarchical - choose to display a regular heatmap.2 or d3heatmap (interactive)
  output$plotUI <- renderUI({
    ns <- session$ns
    
    if (input$inPlotHierSparInteractive)
      withSpinner(d3heatmapOutput(ns("plotHierSparInt"), 
                                  height = paste0(input$inPlotHeight, "px"), 
                                  width = paste0(input$inPlotWidth, "px")))
    else
      withSpinner(plotOutput(ns('outPlotHierSpar'), 
                             height = paste0(input$inPlotHeight, "px"), 
                             width = paste0(input$inPlotWidth, "px")))
  })
  
  
  # Pop-overs ----
  
  addPopover(session, 
             ns("alImportance"),
             title = "Variable importance",
             content = helpText.clHierSpar[["alImportance"]],
             trigger = "click")
  
}