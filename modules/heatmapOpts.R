#
# Free-Clust: Shiny app for clustering data
# Author: Maciej Dobrzynski
#
# UI blocks shared by the hierarchical and the sparse hierarchical tabs.
# Both draw the same heatmap with the same appearance and download controls;
# only the clustering behind it differs. Keeping the controls here means a
# change to them is made once rather than twice, and cannot drift between
# the two tabs.
#
# Widget ids are deliberately fixed, so that either module's server reads
# input$slNAcolor, input$selectPalette and the rest exactly as before.

require(shinyBS) # for tooltips
require(pheatmap) # static heatmap
require(heatmaply) # interactive heatmap
require(RColorBrewer) # brewer.pal, via myGetHeatmapColors


# Collect the appearance settings that myHeatmapStyleUI produces, so the
# plotting helpers below take one argument rather than eight. Reads the fixed
# widget ids declared in this file.
#
# Arguments:
# in.input - the calling module's input object

myHeatmapOpts = function(in.input) {
  return(list(
    colHeat  = myGetHeatmapColors(in.input$selectPalette,
                                  in.input$inRevPalette),
    palDend  = in.input$selectPaletteDend,
    showDend = in.input$selectDend,
    colNA    = grey(in.input$slNAcolor),
    colGrid  = ifelse(in.input$chBdispGrid,
                      grey(in.input$slGridColor),
                      NA),
    fontRow  = in.input$inFontX,
    fontCol  = in.input$inFontY
  ))
}


# Title carried by both heatmaps.
myHeatmapTitle = function(in.dist, in.linkage) {
  return(paste("Distance measure: ",
               in.dist,
               "\nLinkage method: ",
               in.linkage))
}


# Cluster assignments as the one-column data frame both heatmaps take as a
# row annotation.
#
# Arguments:
# in.tree     - hclust or dendrogram to cut
# in.nclust   - number of branches to cut it into
# in.rownames - sample names to attach. pheatmap matches its annotation to the
#               matrix by row name, and sparcl does not preserve them, so the
#               static plots pass them in explicitly. heatmaply matches by
#               position and leaves this NULL.

myGetRowAnnotation = function(in.tree, in.nclust, in.rownames = NULL) {
  locAnn = as.data.frame(dendextend::cutree(tree = in.tree,
                                            k = in.nclust))
  names(locAnn) = "cluster"

  if (!is.null(in.rownames))
    rownames(locAnn) = in.rownames

  return(locAnn)
}


# Static heatmap. Note pheatmap wants the hclust object itself, not
# as.dendrogram(x).
#
# Arguments:
# in.dm        - data matrix, samples in rows
# in.hc        - hclust object behind the row dendrogram
# in.nclust    - number of branches to cut into
# in.opts      - list from myHeatmapOpts
# in.colLabels - column labels; NULL keeps the matrix's own colnames
# in.title     - plot title

myPlotHeatmap = function(in.dm,
                         in.hc,
                         in.nclust,
                         in.opts,
                         in.colLabels = NULL,
                         in.title = NA) {
  pheatmap::pheatmap(
    in.dm,
    color = in.opts$colHeat,
    cluster_rows = if (in.opts$showDend) in.hc else FALSE,
    cluster_cols = FALSE,
    cutree_rows = in.nclust,
    annotation_row = myGetRowAnnotation(in.hc, in.nclust, rownames(in.dm)),
    annotation_colors = list(cluster = myGetDendColors(in.opts$palDend,
                                                      in.nclust)),
    annotation_names_row = FALSE,
    labels_col = in.colLabels,
    legend = TRUE,
    annotation_legend = FALSE,
    na_col = in.opts$colNA,
    border_color = in.opts$colGrid,
    fontsize_row = in.opts$fontRow,
    fontsize_col = in.opts$fontCol,
    angle_col = c("45"),
    main = in.title
  )
}


# Interactive counterpart of myPlotHeatmap. heatmaply wants a dendrogram
# where pheatmap wants an hclust.
#
# Arguments: as myPlotHeatmap, with in.dend in place of in.hc

myPlotHeatmaply = function(in.dm,
                           in.dend,
                           in.nclust,
                           in.opts,
                           in.colLabels = NULL,
                           in.title = "") {
  heatmaply::heatmaply(
    in.dm,
    Rowv = if (in.opts$showDend) in.dend else FALSE,
    dendrogram = if (in.opts$showDend) "row" else "none",
    trace = "none",
    colors = in.opts$colHeat,
    labCol = in.colLabels,
    row_side_colors = myGetRowAnnotation(in.dend, in.nclust),
    row_side_palette = myGetDendColors(in.opts$palDend, in.nclust),
    grid_color = in.opts$colGrid,
    na.value = in.opts$colNA,
    cexCol = in.opts$fontCol * 0.1,
    cexRow = in.opts$fontRow * 0.1,
    margins = c(50, 50, 100, 0),
    xaxis_height = 100,
    yaxis_width = 100,
    main = in.title
  )
}


# Controls for the heatmap's appearance: NA shade, grid lines, both palettes,
# label fonts and plot size.
#
# Arguments:
# ns         - the calling module's namespace function
# in.palHeat - palette pre-selected in the heatmap's colour dropdown

myHeatmapStyleUI = function(ns, in.palHeat = 'RdYlBu') {
  tagList(
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
              ticks = TRUE
            )
          )
        ),

        column(
          3,

          selectInput(
            ns("selectPalette"),
            label = "Heatmap's colour palette:",
            choices = l.col.pal,
            selected = in.palHeat
          ),

          checkboxInput(ns('inRevPalette'),
                        'Reverse colour palette',
                        TRUE)
        ),

        column(
          3,

          selectInput(
            ns("selectPaletteDend"),
            label = "Dendrogram's colour palette",
            choices = l.col.pal.dend,
            selected = 'Color Blind'
          ),

          checkboxInput(ns('selectDend'),
                        'Plot dendrogram and re-order samples',
                        TRUE)
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
        column(
          3,
          numericInput(
            ns('inPlotHeight'),
            'Plot height',
            value = 1200,
            min = 100,
            step = 100
          )
        ),
        column(
          3,
          numericInput(
            ns('inPlotWidth'),
            'Plot width',
            value = 800,
            min = 100,
            step = 100
          )
        )
      )
    )
  )
}


# Controls for downloading the cluster assignments, the dendrogram object and
# the rendered plot. The two tabs name these outputs differently, so the ids
# are arguments rather than fixed.
#
# Arguments:
# ns          - the calling module's namespace function
# in.idClAss  - output id of the cluster assignment download
# in.idDend   - output id of the dendrogram download
# in.idPlot   - id of the nested downPlot module
# in.helpText - named vector supplying the downClAss and downDend tooltips

myHeatmapDownloadUI = function(ns,
                               in.idClAss,
                               in.idDend,
                               in.idPlot,
                               in.helpText) {
  tagList(
    checkboxInput(ns('chBdownload'),
                  'Download plot or data',
                  FALSE),

    conditionalPanel(
      condition = "input.chBdownload",
      ns = ns,

      fluidRow(
        column(
          3,
          downloadButton(ns(in.idClAss), 'Cluster assignments'),
          bsTooltip(
            ns(in.idClAss),
            in.helpText[["downClAss"]],
            placement = "top",
            trigger = "hover",
            options = NULL
          )
        ),
        column(
          3,
          downloadButton(ns(in.idDend), 'Dendrogram object'),
          bsTooltip(
            ns(in.idDend),
            in.helpText[["downDend"]],
            placement = "top",
            trigger = "hover",
            options = NULL
          )
        )
      ),

      downPlotUI(ns(in.idPlot), "")
    )
  )
}
