# Retired modules

Code kept for reference that the app no longer loads. Nothing here is sourced
by `global.R`, so these files are not syntax-checked or run when FreeClust
starts, and they will drift from the rest of the codebase.

## tabBayClust.R

Bayesian clustering, built on [bclust](https://cran.r-project.org/src/contrib/Archive/bclust/).

Retired because none of its three dependencies are installable from CRAN any
more:

- `bclust` - archived, available only from the CRAN archive
- `d3heatmap` - archived; the rest of the app moved to `heatmaply` for
  interactive heatmaps
- `gplots` - only `heatmap.2` was used, which `pheatmap` now covers

Reviving it means installing `bclust` from the archive:

```r
packageurl <- "https://cran.r-project.org/src/contrib/Archive/bclust/bclust_1.5.tar.gz"
install.packages(packageurl, repos = NULL, type = "source")
```

then porting the two heatmaps onto `pheatmap` and `heatmaply` the way
`tabHier.R` does, moving the module to `modules/`, and restoring the three
call sites that are commented out in `global.R`, `ui.R` and `server.R`.

Note that the module predates several conventions the live modules now follow:
it uses the superseded `callModule` signature rather than `moduleServer`, and
it builds its dendrogram palette with a direct `ggthemes::tableau_color_pal`
call, which yields `NA` colours when the cluster count exceeds the palette -
`myGetDendColors` in `modules/auxfn.R` exists to avoid exactly that.
