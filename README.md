# FreeClust: clustering made easy

An open source web-app for easy interactive clustering and cluster validation. Choose from several clustering algortihms, play with parameters and plot results in a fully interactive fashion. Clustering algortihms include: classical hierarchical clustering, sparse hierarchical clustering and a model-based Bayesian approach tailored to cluster high-dimensional data (with many more variables than samples). Validation module allows to estimate the optimal number of clusters and to assess the quality of performed clustering.

FreeClust was published in [Analytical Chemistry](https://pubs.acs.org/doi/abs/10.1021/acs.analchem.7b02221).

#### Running on the server

A running instance can be accessed at [shinyapps.io](https://macdobry.shinyapps.io/free-clust/).

#### Running locally from RStudio

After downloading the source code, open `server.R` or `ui.R` and click `Run App` button in the upper right corner of the window with the code. All missing packages should be downloaded and installed automatically during the first run of the app.

The app uses the follwoing packages:

- magrittr (provides %>% operator)
- shiny
- shinyjs
- shinyBS
- shinycssloaders
- data.table (for fast data processing)
- gplots (provides `heatmap.2`)
- d3heatmap (provides interactive `d3heatmap`)
- ggplot2 (for plotting in cluster validation module)
- dendextend (provides `color_branches`)
- RColorBrewer (provides `brewer.pal`)
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

The package `bclust` for Bayesian clustering is not available on CRAN anymore. The `FreeClust` code contains a functional module/tab that uses this clustering approach. If you want to use it install the package from the [archive](https://cran.r-project.org/src/contrib/Archive/bclust/):

```
packageurl <- "https://cran.r-project.org/src/contrib/Archive/bclust/bclust_1.5.tar.gz"
install.packages(packageurl, repos=NULL, type="source")
```

#### Running from the command-line

In the command-line of your operating system, e.g. Terminal.app on macOS, go to the folder with your app and type:

```
R -e "shiny::runApp('.', port = 3833)"
```

You should see the message:

```
Listening on http://127.0.0.1:3833
```

One you point your web browser to that address, you should see the app running. The port option should be a number above 1000. Choose something that does not overlap with your existing services.


#### About

The web-app integrates several clustering algorithms. These include a widely-used [hierarchical clustering](https://en.wikipedia.org/wiki/Hierarchical_clustering) ([hclust](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/hclust.html)) with a choice of commonly used linkage methods to construct the tree diagram (dendrogram). Additionally, sparse hierarchical clustering ([sparcl](https://cran.r-project.org/web/packages/sparcl/)) and a model-based Bayesian approach, [bclust](https://cran.r-project.org/web/packages/bclust/index.html), tailored to cluster high-dimensional data (with many more variables than samples). The sparse hierarchical and Bayesian clustering provide the information about the importance of a specific categories across the samples.

The app allows for rudimentary data manipulation, such as rescaling, removal of missing data, trimming/clipping outliers. The sequence of these operations applied to data is indicated by numbers in brackets in the UI. 

   - First, data can be rescaled with one of the several methods. In case of *z-score*, each measurement (feature) has its mean subtracted and the result is divided by row’s standard deviation. This type of rescaling is recommended if measurements (features) are not is the same units. Calculating the z-score allows to compare such measurements across samples. If data covers several orders of magnitude, consider taking *log10(x)* or *log10(x+1)*. The latter is especially useful if data naturally covers the range *[0, +inf)*. 
   - Second, users can convert missing values to zeroes which, for example, is necessary to use Bayesian clustering. In general, it is important to understand the causes of missing data and treat them accordingly. Viable options include omitting such samples entirely or replacing missing data with imputed values, the population mean, etc. 
   - Third, data can be *trimmed* to omit values below and above a threshold. Such data points are turned into missing values, however these are no longer subject to conversion of missing values that can take place in step 2. 
   - Fourth, data clipping can assign threshold values to data exceeding (from below or above) these thresholds. 

#### Data format

Users can upload a text file in a wide CSV format: samples and measurements/features should be arranged in columns and rows. The first column and the first row should contain names of samples and/or features, depending on the arrangement. The data can be transposed in the UI. An example file is in the *example-data* folder.

**Warning!** Depending on regional settings (i.e. *locale*), Excel might save the CSV file differently from the default setting where columns are separated by a comma and dot is used as a decimal separator. For example, a German locale would result in a CSV file with columns separated by a semicolon and decimal point signified by a comma. The app can account for such variations of the input format as well as the convention used to represent missing values, e.g. whether it is a character string “NA”, a dash “-“, or an empty space.