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
# clustHierSpar('TabClustHierSpar', dataMod)
# where dataMod is the output from a reactive function 
# that returns a dataset in wide format ready for clustering


require(pheatmap)
require(RColorBrewer) # brewer.pal
require(heatmaply) # interactive heatmap
require(sparcl) # sparse hierarchical and k-means
require(shinyBS) # for tooltips
require(shinycssloaders) # for loader animations


helpText.clHierSpar = c(alImportance = paste0("<p>Weight factors (WF) calculated during clustering ",
                                              "reflect the importance of features/measurements for clustering. ",
                                              "The following label prefixes indicate the importance:",
                                              "<li>no prefix - feature not taken into account</li>",
                                              "<li><p>* - low, WF∈(0, 0.1]</p></li>",
                                              "<li><p>** - medium, WF∈(0.1, 0.5]</p></li>",
                                              "<li><p>*** - high, WF∈(0.5, 1.0]</p></li>",
                                              "</p><p>Witten and Tibshirani (2010): ",
                                              "<i>A framework for feature selection in clustering</i>; ",
                                              "Journal of the American Statistical Association 105(490): 713-726.</p>"),
                        downClAss = "Download a CSV with cluster assignments to time series ID",
                        downDend = "Download an RDS file with dendrogram object. Read later with readRDS() function.",
                        butCluster = paste0("Run sparse hierarchical clustering. This is the slowest step in the app, ",
                                            "so it is not repeated on its own: click again after changing the ",
                                            "dissimilarity measure, the linkage method, or the advanced options. ",
                                            "Loading or rescaling the data clears the result."))


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
        2,

        actionButton(ns('butCluster'), 'Cluster!'),
        bsTooltip(ns("butCluster"),
                  helpText.clHierSpar[["butCluster"]],
                  placement = "top",
                  trigger = "hover")
      ),

      column(
        4,

        sliderInput(
          ns('slNclust'),
          'Number of dendrogram branches to cut',
          min = 1,
          max = MAXNCLUST,
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
                 )
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
                        selected = 1)
               )
             )
             # No alert anchor here: sparcl clusters data containing NAs,
             # so this tab intentionally performs no missing-value check.
      )
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
      )
    ),
    
    myHeatmapStyleUI(ns, 'Spectral'),

    myHeatmapDownloadUI(ns, 'downClAssSpar', 'downDendSpar',
                        'downPlotHierSparPNG', helpText.clHierSpar),
    
    checkboxInput(ns('inPlotHierSparInteractive'), 
                  'Interactive Plot',  
                  value = FALSE),
    uiOutput(ns("plotUI"))
  )
}

# SERVER ----
clustHierSpar <- function(id, dataMod) {
  moduleServer(id, function(input, output, session) {

  ns = session$ns

  
  # Return the number of clusters from the slider 
  # and delay by a constant in milliseconds defined in auxfunc.R
  returnNclust = reactive({
    return(input$slNclust)
  }) %>% debounce(MILLIS)

  # A dendrogram cannot be cut into more branches than there are samples,
  # so cap the slider at whichever is smaller.
  observe({
    myDebug('tabHierSpar:observe:updateSliderInput\n')

    locDM = dataMod()

    if (is.null(locDM))
      return(NULL)

    updateSliderInput(session,
                      'slNclust',
                      max = min(MAXNCLUST, nrow(locDM)))
  })
  
  # Permuting is by far the slowest computation in the app, and it depends on
  # four inputs, so leaving it reactive means a full re-run on every parameter
  # nudge. Hold the result instead and recompute it only when asked.
  locClustRes <- reactiveVal(NULL)

  # A result only describes the data it was computed from. Drop it whenever the
  # data changes - including a rescale or a trim applied in the Histogram tab -
  # so that a dendrogram is never drawn over values it did not come from.
  observeEvent(dataMod(), {
    myDebug('tabHierSpar:observeEvent:dataMod\n')

    locClustRes(NULL)
  }, ignoreNULL = FALSE)

  observeEvent(input$butCluster, {
    myDebug('tabHierSpar:observeEvent:butCluster\n')

    locDM = dataMod()

    if (is.null(locDM)) {
      locClustRes(NULL)
      return(NULL)
    }

    # The permutation step is randomised, so the same data and settings would
    # otherwise give a different answer on every click. Seed it, and restore
    # the session's RNG state afterwards.
    sparsehc <- myWithSeed({
      perm.out <- HierarchicalSparseCluster.permute(
        locDM,
        wbounds = NULL,
        nperms = input$inHierSparNperms,
        dissimilarity = input$selectDist
      )

      HierarchicalSparseCluster(
        dists = perm.out$dists,
        wbound = perm.out$bestw,
        niter = input$inHierSparNiter,
        method = input$selectLinkage,
        dissimilarity = input$selectDist
      )
    })

    locClustRes(sparsehc)
  })

  calcHierSpar <- reactive({
    myDebug('tabHierSpar:calcHierSpar\n')

    return(locClustRes())
  })
  
  
  calcDend <- reactive({
    myDebug('tabHierSpar:calcDend\n')
    
    loc.hc = calcHierSpar()
    if (is.null(loc.hc))
      return(NULL)

    dend <- as.dendrogram(loc.hc[["hc"]])

    return(dend)
  })
  
  # return all IDs (created in dataMod)
  # used when saving cluster associations in sparse hierarchical
  # sparsehc doesn't return original rownames after clustering
  getDataIDs <- reactive({
    myDebug('tabHierSpar:getDataIDs\n')
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
      # returnNclust(), not input$slNclust: the plot is drawn from the
      # debounced value, and the download has to describe the same cut.
      fwrite(x = myGetDataClSpar(calcDend(),
                                 returnNclust(),
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
    myDebug('tabHierSpar:plotHierSpar\n')
    
    locDM = dataMod()
    locHC = calcHierSpar()
    
    validate(
      need(!is.null(locDM), "Nothing to plot. Load data first!"),
      need(!is.null(locHC), "Click Cluster! to run sparse hierarchical clustering.")
    )
    
    # Set colors palette for the heatmap
    locColorHM = myGetHeatmapColors(input$selectPalette, input$inRevPalette)
    
    # number of clusters at which dendrogram is cut
    locNclust = returnNclust()
    
    # make a palette for the dendrogram with the amount of colours equal to the number of clusters
    locColorDend = myGetDendColors(input$selectPaletteDend, locNclust)
    
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
      locClustRows = locHC[["hc"]]
    } else {
      locClustRows = FALSE
    }
    
    
    pheatmap::pheatmap(
      locDM,
      color = locColorHM,
      cluster_rows = locClustRows,
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
  downPlot("downPlotHierSparPNG", createFnameHeatMap, plotHierSpar)
  
  
  # Sparse Hierarchical clustering (sparcl) interactive version
  output$outPlotInt <- renderPlotly({
    myDebug('tabHierSpar:outPlotInt\n')
    
    locDM = dataMod()
    locHC = calcHierSpar()
    
    validate(
      need(!is.null(locDM), "Nothing to plot. Load data first!"),
      need(!is.null(locHC), "Click Cluster! to run sparse hierarchical clustering.")
    )
    
    # Set colors palette for the heatmap
    locColorHM = myGetHeatmapColors(input$selectPalette, input$inRevPalette)
    
    # number of clusters at which dendrogram is cut
    locNclust = returnNclust()
    
    # make a palette for the dendrogram with the amount of colours equal to the number of clusters
    locColorDend = myGetDendColors(input$selectPaletteDend, locNclust)
    
    # Create row-side annotations
    locDend = as.dendrogram(locHC[["hc"]])
    locRowAnnotation <- as.data.frame(
      dendextend::cutree(tree = locDend, 
                         k = locNclust))
    names(locRowAnnotation) = "cluster"
    
    # prepend column names with weights from sparcl
    locColNames = paste0(ifelse(locHC$ws == 0, "",
                                ifelse(
                                  locHC$ws <= 0.1,
                                  "* ",
                                  ifelse(locHC$ws <= 0.5, "** ", "*** ")
                                )), colnames(locDM))
    
    
    
    if (input$selectDend) {
      locRowv = locDend
      locDendType = "row"
    } else {
      locRowv = FALSE
      locDendType = "none"
    }
    
    heatmaply(
      locDM, 
      Rowv = locRowv,
      dendrogram = locDendType,
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

  })
}