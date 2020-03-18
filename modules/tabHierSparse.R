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


require(pheatmap)
require(dendextend) # color_branches
require(RColorBrewer) # brewer.pal
require(heatmaply) # interactive heatmap
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
        4,
        
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
      
      column(6,
             fluidRow(
               column(
                 6,
                 selectInput(
                   ns("selectDist"),
                   label = ("Dissimilarity measure"),
                   choices = list("Euclidean" = "squared.distance",
                                  "Manhattan" = "absolute.value"),
                   selected = 1
                 ),
               ),
               
               column(6,
                      selectInput(
                        ns("selectLinkage"),
                        label = ("Linkage method"),
                        choices = list(
                          "Average"  = "average",
                          "Complete" = "complete",
                          "Single"   = "single",
                          "Centroid" = "centroid"
                        ),
                        selected = 1),
               ),
             ),
             bsAlert("alertAnchorClHierNAsPresent"),
      ),
    ),
    
    br(),
    checkboxInput(ns('chBadvOpto'),
                  'Advanced options',
                  FALSE),
    
    # Only show this panel if chBadvOpto == TRUE
    conditionalPanel(
      condition = "input.chBadvOpto == 1",
      ns = ns, 
      sliderInput(
        ns('inHierSparNperms'),
        'Number of permutations',
        min = 1,
        max = 20,
        value = 2,
        step = 1,
        ticks = TRUE
      ),
      sliderInput(
        ns('inHierSparNiter'),
        'Number of iterations',
        min = 1,
        max = 50,
        value = 15,
        step = 1,
        ticks = TRUE
      ),
    ),
    
    checkboxInput(ns('chBplotStyle'),
                  'Adjust plot appearance',
                  FALSE),
    conditionalPanel(
      condition = "input.chBplotStyle",
      ns = ns,
      
      fluidRow(
        
        column(4,
               sliderInput(
                 ns('slNAcolor'),
                 'Shade of grey for NA values',
                 min = 0,
                 max = 1,
                 value = 0.8,
                 step = .1,
                 ticks = TRUE
               ),
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
          checkboxInput(ns('inRevPalette'), 'Reverse colour palette', TRUE),
        ),
        column(
          3,
          selectInput(
            ns("selectPaletteDend"),
            label = "Dendrogram\'s colour palette",
            choices = l.col.pal.dend,
            selected = 'Color Blind'
          ),
          
          checkboxInput(ns('selectDend'), 'Plot dendrogram and re-order samples', TRUE),
        )
      ),
      
      fluidRow(
        column(
          2,
          numericInput(
            ns('inFontX'),
            'Font size row labels',
            10,
            min = 1,
            width = 100,
            step = 1
          )
        ),
        column(
          2,
          numericInput(
            ns('inFontY'),
            'Font size column labels',
            10,
            min = 1,
            width = 100,
            step = 1
          )
        ),
        column(3,
               numericInput(
                 ns('inPlotHeight'),
                 'Plot height',
                 value = 800,
                 min = 100,
                 step = 100
               )
        ),
        column(3,
               numericInput(
                 ns('inPlotWidth'),
                 'Plot width',
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
               
               downloadButton(ns('downClAssSpar'), 'Cluster assignments'),
               bsTooltip(ns("downClAssSpar"),
                         helpText.clHierSpar[["downClAss"]],
                         placement = "top",
                         trigger = "hover",
                         options = NULL)
        ),
        
        column(3,
               
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
    cat(file = stdout(), 'tabHierSpar:calcHierSpar\n')
    
    locDM = dataMod()
    if (is.null(locDM))
      return(NULL)
    
    perm.out <- HierarchicalSparseCluster.permute(
      locDM,
      wbounds = NULL,
      nperms = input$inHierSparNperms,
      dissimilarity = input$selectDist
    )
    
    sparsehc <- HierarchicalSparseCluster(
      dists = perm.out$dists,
      wbound = perm.out$bestw,
      niter = input$inHierSparNiter,
      method = input$selectLinkage,
      dissimilarity = input$selectDist
    )
    return(sparsehc)
  })
  
  
  calcDend <- reactive({
    cat(file = stdout(), 'tabHierSpar:calcDend\n')
    
    loc.hc = calcHierSpar()
    if (is.null(loc.hc))
      return(NULL)
    
    # number of clusters at which dendrogram is cut
    loc.k = returnNclust()
    
    # make a palette with the amount of colours equal to the number of clusters
    loc.col = ggthemes::tableau_color_pal(input$selectPaletteDend)(n = loc.k)
    
    
    dend <- as.dendrogram(loc.hc[["hc"]])
    dend <- color_branches(dend, 
                           col = loc.col,
                           k = loc.k)
    
    return(dend)
  })
  
  # return all IDs (created in dataMod)
  # used when saving cluster associations in sparse hierarchical
  # sparsehc doesn't return original rownames after clustering
  getDataIDs <- reactive({
    cat(file = stdout(), 'tabHierSpar:getDataIDs\n')
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
             input$selectDist,
             '_',
             input$selectLinkage, '.csv')
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

  plotHierSpar <- function() {
    cat(file = stdout(), 'tabHierSpar:plotHierSpar\n')
    
    locDM = dataMod()
    locHC = calcHierSpar()
    
    validate(
      need(!is.null(locDM), "Nothing to plot. Load data first!"),
      need(!is.null(locHC), "Did not perform clustering.")
    )
    
    # Set colors palette for the heatmap
    if (input$inRevPalette)
      locColorHM <-
      rev(colorRampPalette(brewer.pal(9, input$selectPalette))(n = 99))
    else
      locColorHM <-
      colorRampPalette(brewer.pal(9, input$selectPalette))(n = 99)
    
    # number of clusters at which dendrogram is cut
    locNclust = returnNclust()
    
    # make a palette for the dendrogram with the amount of colours equal to the number of clusters
    locColorDend = ggthemes::tableau_color_pal(input$selectPaletteDend)(n = locNclust)
    names(locColorDend) = seq(1, locNclust, 1)
    
    # Create row-side annotations
    locRowAnnotation <- as.data.frame(
      dendextend::cutree(tree = locHC[["hc"]], 
                         k = locNclust))
    names(locRowAnnotation) = "cluster"
    
    # for some reason row names are not preserved,
    # add them from the original dm
    rownames(locRowAnnotation) = rownames(locDM)
    
    # prepend column names with weights from sparcl
    locColNames = paste0(ifelse(locHC$ws == 0, "",
                                 ifelse(
                                   locHC$ws <= 0.1,
                                   "* ",
                                   ifelse(locHC$ws <= 0.5, "** ", "*** ")
                                 )), colnames(locDM))
    
    # pheatmap accepts direct output from hclust,
    # NOT as.dendrogram(x)
    if (input$selectDend) {
      assign("locVarTmp1", locHC[["hc"]])
    } else {
      assign("locVarTmp1", FALSE)
    }
    
    
    pheatmap::pheatmap(
      locDM,
      color = locColorHM,
      cluster_rows = locVarTmp1,
      cluster_cols = FALSE,
      cutree_rows = locNclust,
      annotation_row = locRowAnnotation,
      annotation_colors = list(cluster = locColorDend),
      annotation_names_row = F, 
      labels_col = locColNames,
      legend = T, 
      annotation_legend = F,
      na_col = grey(input$slNAcolor),
      border_color = ifelse(input$chBdispGrid, 
                            grey(input$slGridColor),
                            NA),
      fontsize_row = input$inFontX,
      fontsize_col = input$inFontY,
      angle_col = c("45"),
      main = paste(
        "Distance measure: ",
        input$selectDist,
        "\nLinkage method: ",
        input$selectLinkage
      )
    )

  }
  
  # Sparse Hierarchical - display plot
  output$outPlotHierSpar <- renderPlot({
    plotHierSpar()
  })
  
  createFnameHeatMap = reactive({
    
    paste0('clust_hierSpar_',  
           input$selectDist,
           "_",
           input$selectLinkage, 
           '.png')
    
  })
  
  # Sparse Hierarchical - download png
  callModule(downPlot, "downPlotHierSparPNG", createFnameHeatMap, plotHierSpar)
  
  
  # Sparse Hierarchical clustering (sparcl) interactive version
  output$outPlotInt <- renderPlotly({
    cat(file = stdout(), 'tabHierSpar:outPlotInt\n')
    
    locDM = dataMod()
    locHC = calcHierSpar()
    
    validate(
      need(!is.null(locDM), "Nothing to plot. Load data first!"),
      need(!is.null(locHC), "Did not perform clustering")
    )
    
    # Set colors palette for the heatmap
    if (input$inRevPalette)
      locColorHM <-
      rev(colorRampPalette(brewer.pal(9, input$selectPalette))(n = 99))
    else
      locColorHM <-
      colorRampPalette(brewer.pal(9, input$selectPalette))(n = 99)
    
    # number of clusters at which dendrogram is cut
    locNclust = returnNclust()
    
    # make a palette for the dendrogram with the amount of colours equal to the number of clusters
    locColorDend = ggthemes::tableau_color_pal(input$selectPaletteDend)(n = locNclust)
    names(locColorDend) = seq(1, locNclust, 1)
    
    # Create row-side annotations
    locDend = as.dendrogram(locHC[["hc"]])
    locRowAnnotation <- as.data.frame(
      dendextend::cutree(tree = locDend, 
                         k = locNclust))
    names(locRowAnnotation) = "cluster"
    
    # Uncomment to have dendrogram branches coloured the same as side annotations.
    # The static plot has a regular black dendrogram, thus commented here for consistency.
    # locDend <- dendextend::color_branches(locDend, 
    #                                       col = locColorDend,
    #                                       k = locNclust)
    
    # prepend column names with weights from sparcl
    locColNames = paste0(ifelse(locHC$ws == 0, "",
                                ifelse(
                                  locHC$ws <= 0.1,
                                  "* ",
                                  ifelse(locHC$ws <= 0.5, "** ", "*** ")
                                )), colnames(locDM))
    
    
    
    if (input$selectDend) {
      assign("var.tmp.1", locDend)
      var.tmp.2 = "row"
    } else {
      assign("var.tmp.1", FALSE)
      var.tmp.2 = "none"
    }
    
    heatmaply(
      locDM, 
      Rowv = var.tmp.1,
      dendrogram = var.tmp.2,
      trace = "none",
      colors = locColorHM, 
      labCol = locColNames,
      row_side_colors = locRowAnnotation,
      row_side_palette = locColorDend,
      grid_color = ifelse(input$chBdispGrid, 
                          grey(input$slGridColor), 
                          NA), 
      na.value = grey(input$slNAcolor),
      cexCol = input$inFontY * 0.1,
      cexRow = input$inFontX * 0.1,
      margins = c(50, 50, 100, 0),
      xaxis_height = 100,
      yaxis_width = 100,
      main = paste(
        "Distance measure: ",
        input$selectDist,
        "\nLinkage method: ",
        input$selectLinkage
      )
    )
  })
  
  
  
  # Sparse Hierarchical - choose to display a regular heatmap.2 or heatmaply (interactive)
  output$plotUI <- renderUI({
    ns <- session$ns
    
    if (input$inPlotHierSparInteractive)
      plotlyOutput(ns("outPlotInt"), 
                      height = paste0(input$inPlotHeight, "px"), 
                      width = paste0(input$inPlotWidth, "px"))
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