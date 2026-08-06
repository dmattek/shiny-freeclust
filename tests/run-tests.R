#!/usr/bin/env Rscript
#
# Free-Clust: run the test suite.
#
#   Rscript tests/run-tests.R
#
# from the root of the repository. The app is not an R package, so the helpers
# are loaded exactly the way the app loads them, by sourcing global.R, and the
# tests then run against what that leaves behind.

if (!file.exists("global.R"))
  stop("Run this from the repository root: Rscript tests/run-tests.R")

suppressMessages(library(testthat))
suppressMessages(library(shiny))

# Sourcing global.R also installs any missing packages, exactly as launching
# the app would.
suppressMessages(source("global.R"))

# Tracing every reactive would bury the test output.
DEB = FALSE

testthat::test_dir("tests/testthat", stop_on_failure = TRUE)
