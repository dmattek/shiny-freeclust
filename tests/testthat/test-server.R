#
# Free-Clust: tests for the data source handling in server.R
#
# Two buttons feed one dataset: "Synthetic data" and "Load Data". The rule is
# that the most recent click wins, including when the same button is clicked
# again after the other one was used. That used to be arranged by comparing
# each button's click count against a snapshot taken at session start, which
# could not be exercised here at all: the snapshot reads input values that a
# mock session has not got yet. A reactiveVal written by two observers can be.

srvFreeClust = eval(parse(testthat::test_path("..", "..", "server.R")))

# Inputs the sidebar and the histogram module need in order to run quietly,
# so that the assertions below are about the data source and nothing else.
locBaseInputs = list(
  rBflipRowCol = "row",
  rButDataNA = "",
  rButDataSep = ",",
  rButDataDec = ".",
  butDataGen1 = 0,
  butDataLoad = 0,
  `TabDataHist-selRescale` = "noresc",
  `TabDataHist-chBdataNA20` = FALSE,
  `TabDataHist-chBdataTrim` = FALSE,
  `TabDataHist-chBdataClip` = FALSE,
  `TabDataHist-slHistBinN` = 10,
  `TabDataHist-inDataTrimMin` = 0,
  `TabDataHist-inDataTrimMax` = 1e6,
  `TabDataHist-inDataClipMin` = 0,
  `TabDataHist-inDataClipMax` = 1e6
)

# A small, valid CSV in the layout the app expects
makeCsv = function() {
  locPath = tempfile(fileext = ".csv")
  writeLines(c("id,a,b", "s1,1,2", "s2,3,4", "s3,5,6"), locPath)

  return(locPath)
}


test_that("no data is loaded until a button is clicked", {
  shiny::testServer(srvFreeClust, {
    do.call(session$setInputs, locBaseInputs)

    expect_null(dataMod())
  })
})

test_that("either source can load, and the most recent click wins", {
  locCsv = makeCsv()

  shiny::testServer(srvFreeClust, {
    do.call(session$setInputs, locBaseInputs)

    session$setInputs(butDataGen1 = 1)
    expect_equal(dim(dataMod()), c(150, 4))

    session$setInputs(fileDataLoad = list(datapath = locCsv), butDataLoad = 1)
    expect_equal(dim(dataMod()), c(3, 2))
    expect_equal(rownames(dataMod()), c("s1", "s2", "s3"))

    # back to the synthetic data
    session$setInputs(butDataGen1 = 2)
    expect_equal(dim(dataMod()), c(150, 4))

    # and back to the file. Clicking a button again after the other one was
    # used has to work; this is what the click counters existed to arrange.
    session$setInputs(butDataLoad = 2)
    expect_equal(dim(dataMod()), c(3, 2))
  })
})

test_that("a refused file clears the data rather than leaving the old one", {
  locCsv = makeCsv()

  shiny::testServer(srvFreeClust, {
    do.call(session$setInputs, locBaseInputs)

    session$setInputs(fileDataLoad = list(datapath = locCsv), butDataLoad = 1)
    expect_equal(dim(dataMod()), c(3, 2))

    # clicking Load with nothing selected is refused, and says so
    session$setInputs(fileDataLoad = NULL, butDataLoad = 2)
    expect_null(dataMod())

    # and the other source still works afterwards
    session$setInputs(butDataGen1 = 1)
    expect_equal(dim(dataMod()), c(150, 4))
  })
})

test_that("the row/column switch transposes whatever is loaded", {
  locCsv = makeCsv()

  shiny::testServer(srvFreeClust, {
    do.call(session$setInputs, locBaseInputs)
    session$setInputs(fileDataLoad = list(datapath = locCsv), butDataLoad = 1)

    expect_equal(dim(dataMod()), c(3, 2))
    expect_equal(rownames(dataMod()), c("s1", "s2", "s3"))

    session$setInputs(rBflipRowCol = "col")
    expect_equal(dim(dataMod()), c(2, 3))
    expect_equal(rownames(dataMod()), c("a", "b"))

    session$setInputs(rBflipRowCol = "row")
    expect_equal(dim(dataMod()), c(3, 2))
  })
})

test_that("a file whose columns are not numeric is refused", {
  locPath = tempfile(fileext = ".csv")
  writeLines(c("id,a,label", "s1,1,alpha", "s2,3,beta"), locPath)

  shiny::testServer(srvFreeClust, {
    do.call(session$setInputs, locBaseInputs)
    session$setInputs(fileDataLoad = list(datapath = locPath), butDataLoad = 1)

    expect_null(dataMod())
  })
})

test_that("the loaded data carries a record of how it was read", {
  locCsv = makeCsv()

  shiny::testServer(srvFreeClust, {
    do.call(session$setInputs, locBaseInputs)
    session$setInputs(fileDataLoad = list(datapath = locCsv, name = "mydata.csv"),
                      butDataLoad = 1)

    locProv = myGetProvenance(dataMod())
    expect_true(any(grepl("Data source: mydata.csv", locProv)))
    expect_true(any(grepl("Samples read from: rows", locProv)))

    session$setInputs(rBflipRowCol = "col")
    expect_true(any(grepl("Samples read from: columns",
                          myGetProvenance(dataMod()))))

    # the synthetic data says so too
    session$setInputs(butDataGen1 = 1)
    expect_true(any(grepl("synthetic data", myGetProvenance(dataMod()))))
  })
})

test_that("a downloaded cluster assignment states its settings and still reads back", {
  locCsv = makeCsv()

  locM = as.matrix(iris[1:12, 1:4])
  rownames(locM) = sprintf("s%02d", 1:12)

  shiny::testServer(clustHier, args = list(dataMod = shiny::reactive({
    mySetProvenance(locM,
                    c("Data source: mydata.csv", "[1] Rescaling: zscore"))
  })), {
    session$setInputs(slNclust = 3, selectDist = "euclidean",
                      selectLinkage = "average")
    session$elapse(2 * MILLIS)

    # a download handler under testServer yields the path it wrote to
    locPath = output$downClAss

    locLines = readLines(locPath)
    locHead = grep("^#", locLines, value = TRUE)

    expect_true(any(grepl("Data source: mydata.csv", locHead)))
    expect_true(any(grepl("Rescaling: zscore", locHead)))
    expect_true(any(grepl("Dissimilarity measure: euclidean", locHead)))
    expect_true(any(grepl("Linkage method: average", locHead)))
    expect_true(any(grepl("cut into: 3 clusters", locHead)))

    # the comment header must not stop the file being read again
    locBack = data.table::fread(locPath)
    expect_equal(nrow(locBack), 12)
    expect_equal(names(locBack), c("id", "cl"))
    expect_setequal(locBack$id, rownames(locM))
    expect_setequal(unique(locBack$cl), 1:3)
  })
})

test_that("repeated sample names are made unique rather than refused", {
  locPath = tempfile(fileext = ".csv")
  writeLines(c("id,a,b", "ctrl,1,2", "ctrl,3,4", "treat,5,6"), locPath)

  shiny::testServer(srvFreeClust, {
    do.call(session$setInputs, locBaseInputs)
    session$setInputs(fileDataLoad = list(datapath = locPath), butDataLoad = 1)

    expect_equal(dim(dataMod()), c(3, 2))
    expect_equal(rownames(dataMod()), c("ctrl", "ctrl.1", "treat"))
  })
})
