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
