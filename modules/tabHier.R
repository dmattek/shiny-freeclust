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


require(pheatmap)
require(dendextend) # color_branches
require(RColorBrewer) # brewer.pal
require(heatmaply) # for interactive heatmap
require(shinyBS) # for tooltips
require(shinycssloaders) # for loader animations

helpText.clHier = c(alertNAsPresentClDTW = paste0("NAs (still) present in the dataset. DTW cannot calculate the distance."),
                    alertNAsPresentCl = paste0("NAs (still) present in the dataset, caution recommended."),
                    alertNAsPresentDist = "Calculation of the distance matrix yielded NAs. Dataset might be too sparse",
                    alLearnMore = paste0("<p>Agglomerative <a href=\"https://en.wikipedia.org/wiki/Hierarchical_clustering\" target=\"_blank\" title=\"External link\">hierarchical clustering</a> ",
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
      
      column(4,
             sliderInput(
               ns('slNclust'),
               'Number of dendrogram branches to cut',
               min = 1,
               max = 10,
               value = 1,
               step = 1,
               ticks = TRUE,
               round = TRUE)
      ),
      
      column(6,
             fluidRow(
               column(
                 6,
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
               ),
               
               column(6,
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
                        selected = 1),
               ),
             ),
             bsAlert("alertAnchorClHierNAsPresent"),
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
          4,
          
          sliderInput(
            ns('slNAcolor'),
            'Shade of grey for NA values',
            min = 0,
            max = 1,
            value = 0.8,
            step = .1,
            ticks = TRUE),
          
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
            selected = 'RdYlBu'
          ),
          
          checkboxInput(ns('inRevPalette'), 
                        'Reverse colour palette', 
                        TRUE),
          
        ),
        
        column(
          3,
          
          selectInput(
            ns("selectPaletteDend"),
            label = "Dendrogram\'s colour palette",
            choices = l.col.pal.dend,
            selected = 'Color Blind'
          ),
          
          checkboxInput(ns('selectDend'), 
                        'Plot dendrogram and re-order samples', 
                        TRUE),
          
        ),
      ),
      
      fluidRow(
        column(2,
               numericInput(
                 ns('inFontX'),
                 'Font size row labels',
                 10,
                 min = 1,
                 width = 100,
                 step = 1
               )
        ),
        column(2,
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
               ),
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
               downloadButton(ns('downClAss'), 'Cluster assignments'),
               bsTooltip(ns("downClAss"),
                         helpText.clHier[["downClAss"]],
                         placement = "top",
                         trigger = "hover",
                         options = NULL)
        ),
        column(3,
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
    cat(file = stdout(), 'tabHier:calcDist\n')
    
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
    
    
    locDist = proxy::dist(locDM, 
                          method = input$selectDist)
    
    return(locDist)
  })
  
  calcHC <- reactive({
    cat(file = stdout(), 'tabHier:calcHC\n')
    
    locDist = calcDist()
    
    if (is.null(locDist)) {
      return(NULL)
    } else if (sum(is.na(locDist)) > 0) {
      createAlert(session, "alertAnchorClHierNAsPresent", "alertNAsPresentDist", title = "Error",
                  content = helpText.clHier[["alertNAsPresentDist"]], 
                  append = FALSE, 
                  style = "danger")
      
      return(NULL)
    } else {
      closeAlert(session, "alertNAsPresentDist")
    }
    
    # Perform hierarchical clustering
    locClHc = hclust(locDist, 
                     method = input$selectLinkage)
    
    return(locClHc)
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
      fwrite(x = myGetDataCl(calcHC(), 
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
      saveRDS(object = calcHC(), file = file)
    }
  )
  
  
  
  # Function instead of reactive as per:
  # http://stackoverflow.com/questions/26764481/downloading-png-from-shiny-r
  # This function is used to plot and to download a pdf
  
  plotHier <- function() {
    cat(file = stdout(), 'tabHier:plotHier\n')
    
    locDM = dataMod()
    locHC <- calcHC()
    
    validate(
      need(!is.null(locDM), "Nothing to plot. Load data first!"),
      need(!is.null(locHC), "Did not cluster")
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
      dendextend::cutree(tree = locHC, 
                         k = locNclust))
    names(locRowAnnotation) = "cluster"
    rownames(locRowAnnotation) = rownames(locDM)
    
    # pheatmap accepts direct output from hclust,
    # NOT as.dendrogram(x)
    if (input$selectDend) {
      assign("var.tmp.1", locHC)
    } else {
      assign("var.tmp.1", FALSE)
    }
    
    
    pheatmap::pheatmap(
      locDM,
      color = locColorHM,
      cluster_rows = var.tmp.1,
      cluster_cols = FALSE,
      cutree_rows = locNclust,
      annotation_colors = list(cluster = locColorDend),
      annotation_row = locRowAnnotation, 
      annotation_names_row = F,
      legend = T, 
      annotation_legend = F,
      na_col = grey(input$slNAcolor),
      border_color = ifelse(input$chBdispGrid, 
                            grey(input$slGridColor), 
                            NA),
      fontsize_col = input$inFontY,
      fontsize_row = input$inFontX,
      angle_col = c("45"),
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
  output$outPlotInt <- renderPlotly({
    cat(file = stdout(), 'tabHier:outPlotInt\n')
    
    locDM = dataMod()
    locHC = calcHC()
    
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
    locDend = as.dendrogram(locHC)
    locRowAnnotation <- as.data.frame(
      dendextend::cutree(tree = locDend, 
                         k = locNclust))
    names(locRowAnnotation) = "cluster"
    
    # Uncomment to have dendrogram branches coloured the same as side annotations.
    # The static plot has a regular black dendrogram, thus commented here for consistency.
    # locDend <- dendextend::color_branches(locDend, 
    #                                       col = locColorDend,
    #                                       k = locNclust)
    
    
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
      row_side_colors = locRowAnnotation,
      row_side_palette = locColorDend,
      grid_color = ifelse(input$chBdispGrid, grey(input$slGridColor), NA), 
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
  
  # Hierarchical - choose to display regular heatmap.2 or d3heatmap (interactive)
  output$plotUI <- renderUI({
    ns <- session$ns
    if (input$plotInt)
      plotlyOutput(ns("outPlotInt"), 
                   height = paste0(input$inPlotHeight, "px"), 
                   width = paste0(input$inPlotWidth, "px"))
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