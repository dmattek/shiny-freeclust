# Check if all required packages are installed, if not, attempt to install the missing ones
required_packages = c(
  "data.table",
  "factoextra",
  "dtw",
  "shiny", 
  "shinyjs", 
  "shinyBS", 
  "shinycssloaders",
  "ggplot2", 
  "gplots", 
  "d3heatmap",
  "pheatmap",
  "heatmaply",
  "dendextend", 
  "RColorBrewer", 
  "ggthemes",
  "sparcl"
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
source('modules/tabHist.R')
source('modules/tabHier.R')
source('modules/tabHierSparse.R')
#source('modules/tabBayClust.R')
source('modules/tabClValid.R')
