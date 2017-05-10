# FreeClust: clustering made easy

#### Running on the server
This is source code of an interactive clustering web-app written in R/Shiny. A running instance can be found here:
http://bioz-lcms-chromclust.bioz.unibas.ch:3838/shiny-freeclust

#### Running locally from RStudio
After downloading the source code, open `server.R` or `ui.R` and click `Run App` button in the upper right corner of the window with the code. The following packages need to be installed before running this code:

- shiny
- shinyjs
- gplots (provides `heatmap.2`)
- dendextend (provides `color_branches`)
- RColorBrewer (provides `brewer.pal`)
- d3heatmap (provides interactive `d3heatmap`)
- sparcl (provides sparse hierarchical and k-means clustering)
- bclust (provides Bayesian clustering)

#### About

The web-app integrates several clustering algorithms. These include a widely-used hierarchical clustering (hclust) with a choice of commonly used linkage methods to construct the tree diagram (dendrogram). Additionally, sparse hierarchical clustering (sparcl) and a model-based Bayesian approach tailored to cluster high-dimensional data (with many more variables than samples). The sparse hierarchical and Bayesian clustering provide the information about the importance of a specific categories across the samples.

The web-app allows for rudimentary data manipulation. Users have an option to convert missing values to zeroes which is necessary to use Bayesian clustering. In general, it is important to understand the causes of missing data and treat them accordingly. Viable options include omitting such samples entirely or replacing missing data with imputed values, the population mean, etc. 

Rescaling is another data manipulation option available to users. When switched on, each row has its mean subtracted and the result is divided by row’s standard deviation; this corresponds to calculating z-scores. Taking log10 of data is another option available.Users can also trim data to omit values below and above a threshold. Such data points are turned into missing values, however these are no longer subject to conversion of missing values in source data described above. Also available is data clipping which assigns threshold values to data exceeding (from below or above) these thresholds. Neither trimming nor clipping affects the zeroes resulting from replacement of missing data.

#### Data format
Users can generate artificial random dataset or can upload a text file in CSV format where rows correspond to different categories, and columns correspond to samples. The first column should include labels of categories, while the first row should contain sample names. An example file is in example-data folder.

Depending on regional settings (i.e. locale), Excel might save the CSV file differently from the default setting where columns are separated by a comma and dot is used as a decimal separator. For example, a German locale would result in a CSV file with columns separated by a semicolon and decimal point signified by a comma. The app can account for such variations of the input format as well as the convention used to represent missing values, e.g. whether it is a character string “NA”, a dash “-“, or an empty space.