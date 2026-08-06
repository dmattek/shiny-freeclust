#
# Free-Clust: tests for the data pipeline in modules/tabHist.R
#
# The histogram tab is where the data is actually prepared: everything the
# clustering tabs receive has been through its rescale, NA, trim and clip
# steps, in that order. These tests drive the real module and check what it
# hands on.

# Drive dataHist with a matrix and the given inputs, and return what the module
# passes to the clustering tabs.
runPipeline = function(in.dm, ...) {
  locOut = NULL
  locIn = list(selRescale = "noresc",
               chBdataNA20 = FALSE,
               chBdataTrim = FALSE,
               chBdataClip = FALSE,
               slHistBinN = 10,
               inDataTrimMin = 0,
               inDataTrimMax = 1e6,
               inDataClipMin = 0,
               inDataClipMax = 1e6)
  locIn = utils::modifyList(locIn, list(...))

  shiny::testServer(dataHist,
                    args = list(inDataMod = shiny::reactive(in.dm)), {
    do.call(session$setInputs, locIn)
    session$elapse(2 * MILLIS)
    locOut <<- session$getReturned()()
  })

  return(locOut)
}

locDM = matrix(as.numeric(1:20), nrow = 5,
               dimnames = list(sprintf("s%d", 1:5), LETTERS[1:4]))

# The module attaches a record of what it did; drop it when comparing values
dropProv = function(in.dm) mySetProvenance(in.dm, NULL)


test_that("data passes through untouched when nothing is enabled", {
  expect_equal(dropProv(runPipeline(locDM)), locDM)
})

test_that("the pipeline records what it did", {
  locProv = myGetProvenance(runPipeline(locDM,
                                        selRescale = "zscore",
                                        chBdataNA20 = TRUE,
                                        chBdataTrim = TRUE,
                                        inDataTrimMin = 2,
                                        inDataTrimMax = 9))

  expect_true(any(grepl("Rescaling: zscore", locProv)))
  expect_true(any(grepl("Missing values set to zero: yes", locProv)))
  expect_true(any(grepl("Trimming: discard below 2 and above 9", locProv)))
  expect_true(any(grepl("Clipping: none", locProv)))

  # and says so honestly when nothing was applied
  locProv = myGetProvenance(runPipeline(locDM))
  expect_true(any(grepl("Rescaling: noresc", locProv)))
  expect_true(any(grepl("Trimming: none", locProv)))

  # the lines are usable as a CSV comment header
  expect_true(all(grepl("^# ", myFormatProvenance(locProv))))
})

test_that("NULL data in gives NULL out", {
  expect_null(runPipeline(NULL))
})

test_that("z-score rescales each feature separately", {
  locOut = runPipeline(locDM, selRescale = "zscore")

  # column-wise, so every column has mean 0 and sd 1
  expect_equal(unname(colMeans(locOut)), rep(0, ncol(locDM)))
  expect_equal(unname(apply(locOut, 2, sd)), rep(1, ncol(locDM)))
  expect_equal(dim(locOut), dim(locDM))
  expect_equal(rownames(locOut), rownames(locDM))
})

test_that("log rescaling turns out-of-domain values into NAs", {
  locNeg = matrix(c(-5, 1, 10, 100), nrow = 2)

  locOut = runPipeline(locNeg, selRescale = "log10x")
  expect_true(is.na(locOut[1, 1]))
  expect_equal(locOut[2, 1], log10(1))

  # log10(x + 1) accepts 0 and everything above -1
  locOut = runPipeline(matrix(c(0, 1, 10, 100), nrow = 2), selRescale = "log10xp1")
  expect_false(any(is.na(locOut)))
  expect_equal(locOut[1, 1], log10(1))
})

test_that("missing values become zeroes only when asked", {
  locNA = locDM
  locNA[2, 2] = NA

  expect_true(is.na(runPipeline(locNA)[2, 2]))
  expect_equal(runPipeline(locNA, chBdataNA20 = TRUE)[2, 2], 0)
})

test_that("trimming discards out-of-range values", {
  locOut = runPipeline(locDM, chBdataTrim = TRUE,
                       inDataTrimMin = 5, inDataTrimMax = 15)

  expect_true(all(is.na(locOut[locDM < 5])))
  expect_true(all(is.na(locOut[locDM > 15])))
  expect_equal(locOut[locDM >= 5 & locDM <= 15],
               locDM[locDM >= 5 & locDM <= 15])
})

test_that("clipping caps out-of-range values instead of discarding them", {
  locOut = runPipeline(locDM, chBdataClip = TRUE,
                       inDataClipMin = 5, inDataClipMax = 15)

  expect_false(any(is.na(locOut)))
  expect_equal(min(locOut), 5)
  expect_equal(max(locOut), 15)
})

test_that("an emptied bound means no limit on that side", {
  # A cleared numeric field arrives as NA. It must not silently disable the
  # whole operation, nor turn the data into NAs.
  locOut = runPipeline(locDM, chBdataTrim = TRUE,
                       inDataTrimMin = 5, inDataTrimMax = NA)
  expect_true(all(is.na(locOut[locDM < 5])))
  expect_equal(max(locOut, na.rm = TRUE), max(locDM))

  locOut = runPipeline(locDM, chBdataClip = TRUE,
                       inDataClipMin = NA, inDataClipMax = 15)
  expect_false(any(is.na(locOut)))
  expect_equal(min(locOut), min(locDM))
  expect_equal(max(locOut), 15)
})

test_that("rescaling happens before trimming", {
  # Numbered [1] and [3] in the UI. With z-scores the values are small, so a
  # trim at 5 on raw data would remove everything; applied after rescaling it
  # removes nothing.
  locOut = runPipeline(locDM, selRescale = "zscore",
                       chBdataTrim = TRUE, inDataTrimMin = -5, inDataTrimMax = 5)

  expect_false(any(is.na(locOut)))
})
