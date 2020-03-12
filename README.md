# FreeClust: clustering made easy

A web-app for easy interactive clustering. Choose your clustering algortihms, play with parameters and plot results in a fully interactive fashion. The set of algortihms includes: classical hierarchical clustering, sparse hierarchical clustering and a model-based Bayesian approach tailored to cluster high-dimensional data (with many more variables than samples). FreeClust was published in [Analytical Chemistry](https://pubs.acs.org/doi/abs/10.1021/acs.analchem.7b02221).

#### Running on the server
This is source code of an interactive clustering web-app written in R/Shiny. A running instance can be accessed at [shinyapps.io](https://macdobry.shinyapps.io/free-clust/).

#### Running locally from RStudio

After downloading the source code, open `server.R` or `ui.R` and click `Run App` button in the upper right corner of the window with the code. The following packages need to be installed before running this code:

- magrittr (provides %>% operator)
- shiny
- shinyjs
- shinyBS
- shinycssloaders
- gplots (provides `heatmap.2`)
- ggplot2 (for plotting)
- dendextend (provides `color_branches`)
- RColorBrewer (provides `brewer.pal`)
- d3heatmap (provides interactive `d3heatmap`)
- data.table (for fast data processing)
- sparcl (provides sparse hierarchical and k-means clustering)
- dtw (provides Dynamic Time Warping)
- factoextra (provides cluster validation)

The complete list of dependencies can also be manually installed from the R console by typing:
```
install.packages(c("shiny", "shinyjs", "shinyBS", "shinycssloaders",
					"ggplot2", "gplots", "d3heatmap",
					"dendextend", "RColorBrewer", "ggthemes",
					"data.table", "sparcl", "dtw", "factoextra")) 
```

#### Running from command-line

In the command-line of your operating system, e.g. Terminal.app on macOS, go to the folder with your app and type:

```
R -e "shiny::runApp('.', port = 3833)"
```

You should see the message:

```
Listening on http://127.0.0.1:3833
```

One you point your web browser to that address, you should see the app running. The port option should be a number above 1000. Choose something that does not overlap with your existing services.

#### A note on Bayesian clustering

The package `bclust` for Bayesian clustering is not available on CRAN anymore. The `FreeClust` code contains a functional module/tab that uses this clustering approach. If you want to use it:

   * install the package from the [archive](https://cran.r-project.org/src/contrib/Archive/bclust/):

```
packageurl <- "https://cran.r-project.org/src/contrib/Archive/bclust/bclust_1.5.tar.gz"
install.packages(packageurl, repos=NULL, type="source")
```

   * uncomment lines of code in `server.R`, `global.R`, and `ui.R`.


#### About

The web-app integrates several clustering algorithms. These include a widely-used hierarchical clustering (hclust) with a choice of commonly used linkage methods to construct the tree diagram (dendrogram). Additionally, sparse hierarchical clustering (sparcl) and a model-based Bayesian approach tailored to cluster high-dimensional data (with many more variables than samples). The sparse hierarchical and Bayesian clustering provide the information about the importance of a specific categories across the samples.

The web-app allows for rudimentary data manipulation. Users have an option to convert missing values to zeroes which is necessary to use Bayesian clustering. In general, it is important to understand the causes of missing data and treat them accordingly. Viable options include omitting such samples entirely or replacing missing data with imputed values, the population mean, etc. 

Rescaling is another data manipulation option available to users. When switched on, each row has its mean subtracted and the result is divided by row’s standard deviation; this corresponds to calculating z-scores. Taking log10 of data is another option available.Users can also trim data to omit values below and above a threshold. Such data points are turned into missing values, however these are no longer subject to conversion of missing values in source data described above. Also available is data clipping which assigns threshold values to data exceeding (from below or above) these thresholds. Neither trimming nor clipping affects the zeroes resulting from replacement of missing data.

#### Data format

Users can generate artificial random dataset or can upload a text file in CSV format where rows correspond to different categories, and columns correspond to samples. The first column should include labels of categories, while the first row should contain sample names. An example file is in example-data folder.

Depending on regional settings (i.e. locale), Excel might save the CSV file differently from the default setting where columns are separated by a comma and dot is used as a decimal separator. For example, a German locale would result in a CSV file with columns separated by a semicolon and decimal point signified by a comma. The app can account for such variations of the input format as well as the convention used to represent missing values, e.g. whether it is a character string “NA”, a dash “-“, or an empty space.