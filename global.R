# Check if all required packages are installed, if not, attempt to install the missing ones
required_packages = c(
  "data.table",
  "factoextra",
  "dtw",
  "shiny", 
  "shinyBS", 
  "shinycssloaders",
  "ggplot2", 
  "pheatmap",
  "heatmaply",
  "RColorBrewer",
  "ggthemes",
  "sparcl",
  # Also required. These are easy to overlook because they are called via ::,
  # or attached only as a side effect of another package, so do not remove them
  # on the grounds that no library() call mentions them.
  "magrittr",    # %>%, attached in tabHist.R
  "proxy",       # proxy::dist, including the DTW metric that dtw registers
  "dendextend",  # dendextend::cutree on hclust/dendrogram objects
  "ggpubr",      # ggpubr::ggline in myNbclust (auxfn.R)
  "plotly"       # renderPlotly/plotlyOutput; so far attached only via heatmaply's Depends
)

missing_packages =
  required_packages[!(required_packages %in% installed.packages()[, "Package"])]

if (length(missing_packages)) {
  cat(paste(
    "Missing packages:",
    paste(missing_packages, collapse = ";"),
    "\nAttempting to install them."
  ))
  install.packages(missing_packages)
}

# The package is not available on CRAN anymore,
# install from the archive https://cran.r-project.org/src/contrib/Archive/bclust/
# if (!("bclust" %in% installed.packages())) {
#   packageurl <- "https://cran.r-project.org/src/contrib/Archive/bclust/bclust_1.5.tar.gz"
#   install.packages(packageurl, repos=NULL, type="source")
# }

source('modules/auxfn.R')
source('modules/downPlot.R')
# heatmapOpts.R uses downPlotUI, so it has to follow downPlot.R
source('modules/heatmapOpts.R')
source('modules/tabHist.R')
source('modules/tabHier.R')
source('modules/tabHierSparse.R')
# Bayesian clustering is retired, see retired/README.md
source('modules/tabClValid.R')
