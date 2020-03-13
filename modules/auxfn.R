# 
# Free-Clust: Shiny app for clustering data
# Author: Maciej Dobrzynski
#
# Auxilary functions & definitions of global constants
#


# Global parameters ----
# number of miliseconds to delay reactions to changes in the UI
# used to delay output from sliders
MILLIS = 1000

# Number of significant digits to display in table stats
SIGNIFDIGITSINTAB = 3

# Number of significant digits used to truncate min/max
SIGNIFDIGITSROUND = 4

# if true, additional output printed to R console
DEB = T

# font sizes in pts for screen display
PLOTFONTBASE = 16
PLOTFONTAXISTEXT = 16
PLOTFONTAXISTITLE = 16
PLOTFONTFACETSTRIP = 20
PLOTFONTLEGEND = 16

# height (in pixels) of ribbon and single traj. plots
PLOTRIBBONHEIGHT = 500 # in pixels
PLOTTRAJHEIGHT = 500 # in pixels
PLOTPSDHEIGHT = 500 # in pixels
PLOTBOXHEIGHT = 500 # in pixels
PLOTSCATTERHEIGHT = 500 # in pixels
PLOTWIDTH = 85 # in percent

# default number of facets in plots
PLOTNFACETDEFAULT = 3



helpText.server = c(
  alDataFormat =  paste0(
    "<p>Accepts a CSV text file in wide format. ",
    "First row should contain sample names; first column should contain feature/measurement names. ",
    "Or vice versa. Rows/columns can be flipped in the UI.</p>"),
  alertNegPresent  = "Data points smaller than or equal to zero are present. Before applying log10(x), such data points will be transformed to NAs.",
  alertNegPresent1 = "Data points smaller than or equal to -1 are present. Before applying log10(x+1), such data points will be transformed to NAs."
)

# list of palettes for the heatmap
l.col.pal = list(
  "Spectral" = 'Spectral',
  "Red-Yellow-Green" = 'RdYlGn',
  "Red-Yellow-Blue" = 'RdYlBu',
  "Greys" = "Greys",
  "Reds" = "Reds",
  "Oranges" = "Oranges",
  "Greens" = "Greens",
  "Blues" = "Blues"
)


# list of palettes for the dendrogram
l.col.pal.dend = list(
  "Colorblind 10" = 'Color Blind',
  "Tableau 10" = 'Tableau 10',
  "Tableau 20" = 'Tableau 20',
  "Classic 10" = "Classic 10",
  "Classic 20" = "Classic 20",
  "Traffic 9" = 'Traffic',
  "Seattle Grays 5" = 'Seattle Grays'
)

## Data processing ----

# Calculate minimum of x and floor the result to sig significant digits
myMin = function(x, sig = 4, na.rm = T) {
  locX = min(x, na.rm = na.rm)
  
  return(floor(locX * 10^sig) / (10^sig))
}

# Calculate maximum of x and ceil the result to sig significant digits
myMax = function(x, sig = 4, na.rm = T) {
  locX = max(x, na.rm = na.rm)
  
  return(ceiling(locX * 10^sig) / (10^sig))
}


# From: https://www.r-bloggers.com/winsorization/
myWinsor1 <- 
function (x, fraction=.05)
{
  if(length(fraction) != 1 || fraction < 0 ||
     fraction > 0.5) {
    stop("bad value for 'fraction'")
  }
  lim <- quantile(x, probs=c(fraction, 1-fraction))
  x[ x < lim[1] ] <- lim[1]
  x[ x > lim[2] ] <- lim[2]
  x
}


myWinsor2 <- function (x, multiple=3)
{
  if(length(multiple) != 1 || multiple <= 0) {
    stop("bad value for 'multiple'")
  }
  med <- median(x, na.rm = TRUE)
  y <- x - med
  sc <- mad(y, center=0, na.rm = TRUE) * multiple
  y[ y > sc ] <- sc
  y[ y < -sc ] <- -sc
  y + med
}

# Generate random dataset for testing.
# Consists of 20 samples (A01-A10, B01-B10) and 20 measurements (A-T).
# Only first measurement, A, differs.
# Should generate two clusters.

myUserDataGen <- function() {
  require("MASS")
  cat(file = stdout(), 'generate data \n')
  # assign result to shared 'dataIn' variable
  loc.x <-
    rbind(mvrnorm(10, c(0, 0), matrix(c(1, 0.9, 0.9, 1), 2, 2)),  
          mvrnorm(10, c(4, 0), matrix(c(1,-0.9,-0.9, 1), 2, 2)))
  loc.x <- cbind(loc.x, matrix(rnorm(20 * 18), 20, 18))
  rownames(loc.x) = c(paste("A", sprintf("%02d", 1:10), sep = ""), paste("B", sprintf("%02d", 1:10), sep = ""))
  colnames(loc.x) =  LETTERS[1:20]
  
  return(loc.x)
}

# Generate iris dataset for testing
myUserDataGenIris = function() {
  locX = as.matrix(iris[,1:4])
  locRowNames = sprintf("%s_%03d", as.vector(iris[,5]), 1:nrow(iris))
  rownames(locX) = locRowNames
  
  return(locX)
}


## Clustering ----

# Return a dt with cell IDs and corresponding cluster assignments depending on dendrogram cut (in.k)
# This one works wth dist & hclust pair
# For sparse hierarchical clustering use getDataClSpar
# Arguments:
# in.dend  - dendrogram; usually output from as.dendrogram(hclust(distance_matrix))
# in.k - level at which dendrogram should be cut

myGetDataCl = function(in.dend, in.k) {
  require(data.table)
  cat(file = stdout(), 'getDataCl \n')
  
  if (is.null(in.dend))
    return(NULL)
  
  if (is.null(in.k) | in.k == 0)
    return(NULL)
  
  loc.m = dendextend::cutree(in.dend, 
                             in.k, 
                             order_clusters_as_data = TRUE)
  #print(loc.m)
  
  # The result of cutree contains named vector with names being cell id's
  # THIS WON'T WORK with sparse hierarchical clustering because there, the dendrogram doesn't have original id's
  loc.dt.cl = data.table(id = names(loc.m),
                         cl = loc.m)
  
  #cat('===============\ndataCl:\n')
  #print(loc.dt.cl)
  return(loc.dt.cl)
}


# Return a dt with cell IDs and corresponding cluster assignments depending on dendrogram cut (in.k)
# This one works with sparse hierarchical clustering!
# Arguments:
# in.dend  - dendrogram; usually output from as.dendrogram(hclust(distance_matrix))
# in.k - level at which dendrogram should be cut
# in.id - vector of cell id's

myGetDataClSpar = function(in.dend, in.k, in.id) {
  require(data.table)
  cat(file = stdout(), 'getDataClSpar \n')
  
  if (is.null(in.dend))
    return(NULL)

  if (is.null(in.k) | in.k == 0)
    return(NULL)
  
  if(is.null(in.id))
    return(NULL)
    
  loc.m = dendextend::cutree(in.dend, 
                             in.k, 
                             order_clusters_as_data = TRUE)
  #print(loc.m)
  
  # The result of cutree contains named vector with names being cell id's
  # THIS WON'T WORK with sparse hierarchical clustering because there, the dendrogram doesn't have original id's
  loc.dt.cl = data.table(id = in.id,
                         cl = loc.m)
  
  #cat('===============\ndataCl:\n')
  #print(loc.dt.cl)
  return(loc.dt.cl)
}


# Cluster validation ----

#Customize factoextra functions to accept dissimilarity matrix from start. Otherwise can't use distance functions that are not in base R, like DTW.
# Inherit and adapt hcut function to take input from UI, used for fviz_clust

myHcut <-
  function(x,
           k = 2,
           isdiss = inherits(x, "dist"),
           hc_func = "hclust",
           hc_method = "average",
           hc_metric = "euclidean") {
    
    if (!inherits(x, "dist")) {
      stop("x must be a distance matrix")
    }
    return(
      factoextra::hcut(
        x = x,
        k = k,
        isdiss = TRUE,
        hc_func = hc_func,
        hc_method = hc_method,
        hc_metric = hc_metric
      )
    )
  }

# Modified from factoextra::fviz_nbclust
# Allow (actually enforce) x to be a distance matrix; no GAP statistics for compatibility

myNbclust <-
  function (x,
            FUNcluster = myHcut,
            method = c("silhouette", "wss"),
            k.max = 10,
            verbose = FALSE,
            barfill = "steelblue",
            barcolor = "steelblue",
            linecolor = "steelblue",
            print.summary = TRUE,
            ...)
  {
    set.seed(123)
    
    if (k.max < 2)
      stop("k.max must bet > = 2")
    
    method = match.arg(method)
    
    if (!inherits(x, c("dist")))
      stop("x should be an object of class dist")
    
    else if (is.null(FUNcluster))
      stop(
        "The argument FUNcluster is required. ",
        "Possible values are kmeans, pam, hcut, clara, ..."
      )
    
    else if (method %in% c("silhouette", "wss")) {
      diss <- x  # x IS ENFORCED TO BE A DISSIMILARITY MATRIX
      
      v <- rep(0, k.max)
      
      if (method == "silhouette") {
        loc.mainlab = "Optimal number of clusters from silhouette analysis"
        loc.ylab <- "Average silhouette width"
        for (i in 2:k.max) {
          clust <- FUNcluster(x, i, ...)
          v[i] <-
            factoextra:::.get_ave_sil_width(diss, clust$cluster)
        }
      }
      else if (method == "wss") {
        loc.mainlab = "Optimal number of clusters from within cluster sum of squares"
        
        loc.ylab <- "Total within cluster sum of squares"
        
        for (i in 1:k.max) {
          clust <- FUNcluster(x, i, ...)
          v[i] <- factoextra:::.get_withinSS(diss, clust$cluster)
        }
      }
      
      df <- data.frame(clusters = as.factor(1:k.max), y = v)
      
      p <- ggpubr::ggline(
        df,
        x = "clusters",
        y = "y",
        group = 1,
        color = linecolor,
        ylab = loc.ylab,
        xlab = "Number of clusters",
        main = loc.mainlab
      )
      
      return(p)
    }
  }

# Custom plotting functions ----


#' Custom ggPlot theme based on theme_bw
#'
#' @param in.font.base
#' @param in.font.axis.text
#' @param in.font.axis.title
#' @param in.font.strip
#' @param in.font.legend
#'
#' @return
#' @export
#'
#' @examples
#'
myGgplotTheme = function(in.font.base = 12,
                          in.font.axis.text = 12,
                          in.font.axis.title = 12,
                          in.font.strip = 14,
                          in.font.legend = 12) {
  loc.theme =
    theme_bw(base_size = in.font.base, base_family = "Helvetica") +
    theme(
      panel.spacing = unit(1, "lines"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(color = "black", size = 0.25),
      axis.text = element_text(size = in.font.axis.text),
      axis.title = element_text(size = in.font.axis.title),
      strip.text = element_text(size = in.font.strip, face = "bold"),
      strip.background = element_blank(),
      legend.key = element_blank(),
      legend.text = element_text(size = in.font.legend),
      legend.key.height = unit(1, "lines"),
      legend.key.width = unit(2, "lines")
    )
  
  return(loc.theme)
}
