
library(Racmacs)
library(testthat)
setwd("~/Dropbox/labbook/packages/Racmacs/Racmacs/")
invisible(lapply(rev(list.files("R", full.names = T)), source))

context("Merging maps")

# Set number of dimensions
num_dim <- 2

logtoraw <- function(x){
  x[!is.na(x)] <- 2^x[!is.na(x)]*10
  x[is.na(x)]  <- "*"
  x[x == "5"]  <- "<10"
  x
}

for(maptype in c("racmap", 'racchart')){

  if(maptype == "racmap"){
    mapmaker  <- acmap
    mapreader <- read.acmap
  } else {
    mapmaker  <- acmap.cpp
    mapreader <- read.acmap.cpp
  }

  map0 <- mapmaker(table = matrix(logtoraw(-1:4),   3, 2))
  map1 <- mapmaker(table = matrix(logtoraw(-1:4+1), 3, 2))
  map2 <- mapmaker(table = matrix(logtoraw(-1:4+2), 3, 2))
  map3 <- mapmaker(table = matrix(logtoraw(-1:4+3), 3, 2))
  map4 <- mapmaker(table = matrix(logtoraw(-1:4+4), 3, 2))


  test_that("Reading in titers from a map", {
    map <- mapreader(test_path("../testdata/testmap_merge.ace"))
    expect_equal(
      titerTableLayers(map0),
      list(unname(titerTable(map0)))
    )
    expect_equal(
      unname(titerTable(map0)),
      matrix(logtoraw(-1:4),   3, 2)
    )
  })

  test_that("Titers from flat maps", {

    expect_equal(unname(titerTable(map2)), matrix(logtoraw(-1:4+2), 3, 2))
    expect_equal(titerTableLayers(map2), list(matrix(logtoraw(-1:4+2), 3, 2)))

  })

  test_that("Merging titers", {

    map13 <- mergeMaps(map1, map3)
    expect_equal(unname(titerTable(map13)), matrix(logtoraw(-1:4+2), 3, 2))
    expect_equal(titerTableLayers(map13), list(
      matrix(logtoraw(-1:4+1), 3, 2),
      matrix(logtoraw(-1:4+3), 3, 2)
    ))

  })

}

# Make some charts that can be merged
chart1 <- acmap.cpp(
  ag_names = paste("Antigen", 1:20),
  sr_names = paste("Sera",    1:10),
  ag_coords = matrix(runif(20*num_dim)*10, 20, num_dim),
  sr_coords = matrix(runif(10*num_dim)*10, 10, num_dim),
  table     = generate_hi(20, 10),
  minimum_column_basis = "none"
)

chart2 <- acmap.cpp(
  ag_names = paste("Antigen", 11:30),
  sr_names = paste("Sera",    1:10),
  ag_coords = matrix(runif(20*num_dim)*10, 20, num_dim),
  sr_coords = matrix(runif(10*num_dim)*10, 10, num_dim),
  table     = generate_hi(20, 10),
  minimum_column_basis = "none"
)

chart3 <- acmap.cpp(
  ag_names = paste("Antigen", 21:40),
  sr_names = paste("Sera",    1:10),
  ag_coords = matrix(runif(20*num_dim)*10, 20, num_dim),
  sr_coords = matrix(runif(10*num_dim)*10, 10, num_dim),
  table     = generate_hi(20, 10),
  minimum_column_basis = "none"
)

# Generating merge reports
test_that("Merge reports", {

  expect_message(mergeReport(chart1, chart2))

})

# Merge tables
test_that("Merge error", {

  expect_error({ mergeMaps(chart1,
                           chart2,
                           method = "merge") })

})

# Table merge
test_that("Merge tables", {

  chart1_nooptimization <- removeOptimizations(cloneMap(chart1))
  chart2_nooptimization <- removeOptimizations(cloneMap(chart2))

  merge12 <- mergeMaps(chart1,
                       chart2,
                       method = "table")

  merge12nooptimizations <- mergeMaps(chart1_nooptimization,
                                      chart2_nooptimization,
                                      method = "table")

  expect_equal(merge12, merge12nooptimizations)

})


# Frozen merge
test_that("Frozen and overlay merge", {

  frozen_merge12 <- mergeMaps(chart1,
                              chart2,
                              method = "frozen")

  overlay_merge12 <- mergeMaps(chart1,
                               chart2,
                               method = "overlay")

  frozen_merge12 <- relaxMap(frozen_merge12)

  expect_equal(mapStress(frozen_merge12), mapStress(overlay_merge12))

})



# Incremental merge
test_that("Incremental merge", {

  incremental_merge12 <- mergeMaps(chart1,
                                   chart2,
                                   method = "incremental",
                                   optimizations = 4)

  expect_equal(numOptimizations(incremental_merge12), 4)
  expect_error({
    mergeMaps(chart1,
              chart2,
              method = "overlay",
              optimizations = 100)
  })

})



