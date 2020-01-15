
library(Racmacs)
invisible(lapply(rev(list.files("R", full.names = T)), source))

# Get a record of the start environment
environment_objects <- ls()

# Load the map and the chart
testthat::context("Subsetting maps")

# Fetch test charts
chart <- read.acmap.cpp(testthat::test_path("../testdata/testmap.ace"))
acmap <- read.acmap(testthat::test_path("../testdata/testmap.ace"))

# Check initial number of antigens and sera
num_antigens <- numAntigens(chart)
num_sera     <- numSera(chart)

# Subset the maps
chart_subset <- subsetMap(chart, antigens = seq_len(num_antigens - 1), sera = 1 + seq_len(num_sera - 1))
acmap_subset <- subsetMap(acmap, antigens = seq_len(num_antigens - 1), sera = 1 + seq_len(num_sera - 1))

testthat::test_that("Original map unaffected",{
  testthat::expect_equal(numAntigens(chart), num_antigens)
  testthat::expect_equal(numAntigens(acmap), num_antigens)
  testthat::expect_equal(numSera(chart), num_sera)
  testthat::expect_equal(numSera(acmap), num_sera)
})

testthat::test_that("Subset of map is correct",{

  testthat::expect_equal(numAntigens(chart_subset), num_antigens-1)
  testthat::expect_equal(numAntigens(acmap_subset), num_antigens-1)
  testthat::expect_equal(numSera(chart_subset), num_sera-1)
  testthat::expect_equal(numSera(acmap_subset), num_sera-1)

  testthat::expect_equal(length(agNames(acmap_subset)), num_antigens-1)
  testthat::expect_equal(length(srNames(acmap_subset)), num_sera-1)
  testthat::expect_equal(length(agNames(chart_subset)), num_antigens-1)
  testthat::expect_equal(length(srNames(chart_subset)), num_sera-1)

  testthat::expect_equal(length(agDates(acmap_subset)), num_antigens-1)
  testthat::expect_equal(length(agDates(chart_subset)), num_antigens-1)

})

# Clean up
rm(list = ls()[!ls() %in% environment_objects])


