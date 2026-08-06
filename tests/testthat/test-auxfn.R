#
# Free-Clust: tests for the helpers in modules/auxfn.R
#
# These cover the parts of the app that are plain functions on matrices, where
# a wrong answer is silent rather than an error: colour vectors that quietly
# contain NAs, rounding that goes the wrong way, cluster tables that lose their
# sample names.

test_that("myMin and myMax round outwards to decimal places", {
  # Away from the data, never into it, so that a slider built from these
  # cannot exclude a real value.
  expect_lte(myMin(c(1.234567, 5)), 1.234567)
  expect_gte(myMax(c(1, 5.987654)), 5.987654)

  expect_equal(myMin(c(1.234567, 5), in.dec = 4), 1.2345)
  expect_equal(myMax(c(1, 5.987654), in.dec = 4), 5.9877)

  # in.dec is decimal places, not significant digits: magnitude is irrelevant
  expect_equal(myMin(12345.678, in.dec = 2), 12345.67)
  expect_equal(myMax(12345.671, in.dec = 2), 12345.68)

  expect_equal(myMin(c(1, NA, 3)), 1)
  expect_equal(myMax(c(1, NA, 3)), 3)
})

test_that("myGetDendColors never returns NA, whatever the cluster count", {
  # A palette shorter than the requested cluster count used to be padded with
  # NAs by ggthemes, which pheatmap then drew as blank annotations.
  for (locPal in unlist(l.col.pal.dend)) {
    for (locN in seq_len(MAXNCLUST)) {
      locCol = myGetDendColors(locPal, locN)

      expect_length(locCol, locN)
      expect_false(any(is.na(locCol)), info = paste(locPal, locN))
      expect_equal(names(locCol), as.character(seq_len(locN)))
    }
  }

  # Within the palette's own length the colours are its own, in order
  expect_equal(unname(myGetDendColors("Color Blind", 6)),
               ggthemes::tableau_color_pal("Color Blind")(6))

  # Beyond it they wrap rather than run out
  locCol = myGetDendColors("Seattle Grays", 8)
  expect_equal(unname(locCol[6:8]), unname(locCol[1:3]))

  expect_null(myGetDendColors("Color Blind", 0))
  expect_null(myGetDendColors(NULL, 4))
})

test_that("myGetHeatmapColors builds a ramp and reverses it", {
  locCol = myGetHeatmapColors("RdYlBu", in.rev = FALSE)

  expect_length(locCol, 99)
  expect_false(any(is.na(locCol)))
  expect_equal(myGetHeatmapColors("RdYlBu", in.rev = TRUE), rev(locCol))
})

test_that("myWithSeed is reproducible and leaves the caller's RNG alone", {
  set.seed(999)
  locBefore = .Random.seed

  expect_equal(myWithSeed(runif(3)), myWithSeed(runif(3)))
  expect_equal(.Random.seed, locBefore)

  # a different seed gives a different draw
  expect_false(isTRUE(all.equal(myWithSeed(runif(3), in.seed = 1),
                                myWithSeed(runif(3), in.seed = 2))))
})

test_that("myNbclust matches factoextra and covers a single cluster", {
  locM = as.matrix(iris[1:30, 1:4])
  rownames(locM) = sprintf("s%02d", 1:30)
  locDist = proxy::dist(locM, method = "euclidean")

  locSil = myNbclust(locDist, method = "silhouette", k.max = 5,
                     hc_method = "average", hc_metric = "euclidean")
  locWss = myNbclust(locDist, method = "wss", k.max = 5,
                     hc_method = "average", hc_metric = "euclidean")

  # the WSS curve starts at one cluster, which factoextra::hcut refuses
  expect_length(locWss$data$y, 5)
  expect_false(any(is.na(locWss$data$y)))
  expect_true(all(diff(locWss$data$y) <= 1e-9))

  # and both agree with the private factoextra helpers they replaced
  for (locK in 2:5) {
    locCl = myHcut(locDist, locK, hc_method = "average",
                   hc_metric = "euclidean")$cluster

    expect_equal(myAveSilWidth(locDist, locCl),
                 factoextra:::.get_ave_sil_width(locDist, locCl))
    expect_equal(myWithinSS(locDist, locCl),
                 factoextra:::.get_withinSS(locDist, locCl))
  }

  expect_equal(locSil$data$y[1], 0)
})

test_that("myGetDataCl keeps sample names, myGetDataClSpar takes them", {
  locM = as.matrix(iris[1:20, 1:4])
  rownames(locM) = sprintf("s%02d", 1:20)
  locHc = hclust(dist(locM), method = "average")

  locCl = myGetDataCl(locHc, 3)
  expect_equal(nrow(locCl), 20)
  expect_setequal(locCl$id, rownames(locM))
  expect_setequal(unique(locCl$cl), 1:3)

  # sparcl drops rownames, so the ids arrive separately
  locClSpar = myGetDataClSpar(as.dendrogram(locHc), 3, rownames(locM))
  expect_equal(nrow(locClSpar), 20)
  expect_setequal(locClSpar$id, rownames(locM))

  expect_null(myGetDataCl(NULL, 3))
  expect_null(myGetDataCl(locHc, 0))
  expect_null(myGetDataClSpar(as.dendrogram(locHc), 3, NULL))
})

test_that("myWinsor2 pulls in outliers and keeps the matrix shape", {
  locM = matrix(c(rep(0, 19), 1000), nrow = 5)

  locOut = myWinsor2(locM)

  expect_equal(dim(locOut), dim(locM))
  expect_lt(max(locOut), 1000)

  # data already within the limit is untouched
  locFlat = matrix(rep(c(1, 2), 10), nrow = 5)
  expect_equal(dim(myWinsor2(locFlat)), dim(locFlat))
})
