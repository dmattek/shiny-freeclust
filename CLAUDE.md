# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project

FreeClust — an R/Shiny web app for interactive clustering (hierarchical, sparse hierarchical) and cluster validation of wide-format CSV data. Published in Analytical Chemistry; deployed at https://macdobry.shinyapps.io/free-clust/.

## Commands

Run the app locally:

```
R -e "shiny::runApp('.')"
```

Or open `freeclust.Rproj` in RStudio and click *Run App* from `server.R`/`ui.R`.

Dependencies are auto-installed on startup: `global.R` diffs `required_packages` against `installed.packages()` and calls `install.packages()` on what is missing. When adding a package, add it to that vector *and* to the README install list.

Run the tests from the repository root:

```
Rscript tests/run-tests.R
```

`tests/testthat/` covers the `auxfn.R` helpers and drives the histogram module's rescale/trim/clip pipeline through `shiny::testServer`. There is no linter config. `misc-scripts/testCl.R` is a scratch script for exercising `sparcl` outside Shiny (gitignored, as are `rsconnect/` and `shiny-contest/`). `test-data/` and `example-data/` hold CSVs for manual testing.

## Architecture

Classic three-file Shiny layout (`global.R`, `ui.R`, `server.R`) plus one file per tab in `modules/`. `global.R` runs the dependency check and then `source()`s every module — **a new module must be sourced there or it will not exist at UI-build time**.

Each module file exports a `xxxUI(id)` returning a `tagList` of `ns()`-namespaced widgets, and a `xxx(id, ...)` server function whose body is wrapped in `moduleServer(id, function(input, output, session) { ... })`. Wiring a new tab means three edits: `source()` in `global.R`, `tabPanel(..., xxxUI('TabId'))` in `ui.R`, `xxx('TabId', dataModProc)` in `server.R`.

Note that the bodies inside `moduleServer` are indented as though they were still top-level in the server function; the conversion from the superseded `callModule` API deliberately left indentation alone to keep that diff readable. `ui.R` and `server.R` return their objects directly rather than through the deprecated `shinyUI()`/`shinyServer()` wrappers.

### Data flow

The single currency between all stages is a **base R matrix with samples in rows and features in columns**, with `rownames` carrying sample IDs. Sample IDs matter — they are what cluster-assignment CSVs are keyed on.

```
dataLoad (fread CSV, 1st col -> rownames) ─┐
                                            ├─> dataInBoth ─> dataMod (optional t()) ─> dataHist module
myUserDataGenIris (iris demo button)      ─┘                                                  │
                                                                                              v
                    clustHier / clustHierSpar / clustValid  <──────────────  dmReturn (reactive)
```

`dataInBoth` (server.R) disambiguates "load file" vs. "synthetic data" by comparing each `actionButton` counter against a `reactiveValues` snapshot taken at session start — the buttons must be read directly, not behind `if`/`else`, or the reactive never fires for the second source. Don't "simplify" this.

**The Histogram tab is the data-processing stage, not just a plot.** `modules/tabHist.R` returns a reactive (`dmReturn`) that every clustering module consumes, applying operations in a fixed order, numbered `[1]`–`[4]` in the UI labels:

1. rescale — z-score / log10(x) / log10(x+1) / winsorize (`myWinsor2`)
2. missing values → 0
3. trim — out-of-range values become `NA`
4. clip — out-of-range values become the range limits

Note the split inside that module: `rescaledData()` (steps 1–2) drives the histogram plot, the min/max/NA summary and the trim/clip input defaults; `dmReturn()` (steps 3–4 on top) is what leaves the module. Changing which one an output reads changes app semantics.

### modules/auxfn.R

Shared constants (ALL-CAPS globals: `MILLIS`, `MAXNCLUST`, `PLOTFONT*`, `SIGNIFDIGITS*`), heatmap/dendrogram palette lists, and helpers:

- `myGetDataCl` vs. `myGetDataClSpar` — cluster assignment tables. `sparcl` drops the original rownames, so the sparse variant takes the ID vector as a third argument; the sparse module also re-attaches `rownames` to the pheatmap row annotation from the original matrix for the same reason.
- `myHcut` / `myNbclust` — reimplementations of `factoextra::hcut` / `fviz_nbclust` that **require** a `dist` object rather than raw data. This exists so non-base metrics (DTW via `proxy::dist`) work at all; `myNbclust` drops the GAP statistic. Its two scoring helpers, `myAveSilWidth` and `myWithinSS`, replace unexported `factoextra:::` internals — keep them public-API only. `myNbclust` also special-cases `k = 1` for the WSS curve, because `factoextra::hcut` rejects it.
- `myGgplotTheme` — the shared ggplot theme; `plotly`/`heatmaply` and `pheatmap` outputs are styled separately.

### Plot downloads

`modules/heatmapOpts.R` holds the appearance and download controls shared by the two hierarchical tabs (`myHeatmapStyleUI`, `myHeatmapDownloadUI`). Widget ids are fixed there, so both module servers read `input$slNAcolor`, `input$selectPalette` and the rest directly — changing an id in that file silently breaks both tabs.

`modules/downPlot.R` is a nested module invoked from inside other modules — `downPlotUI(ns('downPlotHierPNG'), "")` in the UI, `downPlot("downPlotHierPNG", fnameReactive, plotFn)` in the server. Because of that, plot bodies are written as **plain functions** (e.g. `plotHier()`), not reactives, so they can be both `renderPlot`ed and re-executed inside a `pdf()`/`png()` device. The filename reactive's extension (`.pdf` vs `.png`) selects the device and the button label.

## Conventions

- Local variables are prefixed `loc` (`locDM`, `locDist`, `locNclust`); shared helpers are prefixed `my`.
- Input IDs are prefixed by widget type: `sl` slider, `chB` checkbox, `but` action button, `rBut`/`rB` radio, `select`/`sel` select, `in` numeric/file, `al` action link, `down` download.
- Tooltip and popover copy lives in a `helpText.<module>` named character vector at the top of each module (HTML strings), consumed by `bsTooltip` in the UI and `addPopover` at the end of the server function.
- Slider-driven reactives are wrapped in `%>% debounce(MILLIS)` to avoid recomputing clustering on every drag step.
- Tracing is `cat(file = stdout(), 'module:function\n')` at the top of reactives; `DEB` in `auxfn.R` is the intended switch.
- Missing/NA handling is surfaced through `bsAlert`/`createAlert`/`closeAlert` anchors: DTW hard-errors on `NA`s (returns `NULL`), other metrics warn and proceed. New distance measures added to `tabHier.R`/`tabClValid.R` should follow that pattern.
- **The sparse hierarchical tab is exempt.** `sparcl` clusters data containing `NA`s, and that tolerance is part of why the method is offered. `tabHierSparse.R` deliberately performs no NA check — do not add one.

## Inactive code

Bayesian clustering lives in `retired/tabBayClust.R` and is not sourced; see `retired/README.md` for why and for what reviving it would take. Nothing in `retired/` is loaded, so it is never syntax-checked and will drift.

The `dataGen1` eventReactive in `server.R` is dead code and calls a misspelled `myUerDataGenIris()`; the live path is `dataInBoth`, which calls `myUserDataGenIris()` directly.
