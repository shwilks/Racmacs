
library(testthat)
context("Viewing a map")

map <- read.acmap(test_path("../testdata/testmap.ace"))

# Viewing a null map
test_that("Viewing a null map", {

  export.viewer.test(
    RacViewer(
      map = NULL,
      options = list(
        viewer.controls = "shown"
      )
    ),
    filename = "map_null.html"
  )

})

# Viewing a null map
test_that("Viewing a map then reload with no optimizations", {

  widget <- RacViewer(
    map = map,
    options = list(
      viewer.controls = "optimizations"
    )
  )

  map_no_opts <- removeOptimizations(map)
  widget <- htmlwidgets::onRender(
    x      = widget,
    jsCode = "function(el, x, data) { el.viewer.load(JSON.parse(data), { maintain_viewpoint:true }); }",
    data   = as.json(map_no_opts)
  )

  export.viewer.test(
    widget,
    filename = "map_switch_to_no_opts.html"
  )

})

# Viewing a aligned optimizations
test_that("Viewing aligned optimizations", {

  map <- read.acmap(test_path("../testdata/h3map2004.ace"))
  map <- realignOptimizations(map)
  export.viewer.test(
    RacViewer(
      map = map,
      options = list(
        viewer.controls = "optimizations"
      )
    ),
    filename = "map_aligned_optimizations.html"
  )

})

# Viewing maps
test_that("Viewing a map", {

  agCoords(map)[1, ] <- c(5.1, 5.4)
  agFill(map) <- "green"
  x <- view(
    map,
    options = list(
      viewer.controls = "diagnostics",
      show.names = "antigens"
    )
  )

  # map_no_opts <- removeOptimizations(map)
  # widget <- htmlwidgets::onRender(
  #   x      = widget,
  #   jsCode = "function(el, x, data) { el.viewer.load(JSON.parse(data), { maintain_viewpoint:true }); }",
  #   data   = as.json(map_no_opts)
  # )

  expect_equal(class(x), c("RacViewer", "htmlwidget"))
  export.viewer.test(
    x,
    filename = "map_test.html"
  )

})

# Changing point styles
test_that("Viewing a map", {

  map_styled <- map
  srShown(map_styled) <- FALSE
  agShape(map_styled) <- "TRIANGLE"
  x <- view(map_styled)
  expect_equal(class(x), c("RacViewer", "htmlwidget"))
  export.viewer.test(
    x,
    filename = "map_pointstyles.html"
  )

})

# 3D map
test_that("Viewing a 3D map", {

  map <- read.acmap(test_path("../testdata/testmap_h3subset3d.ace"))
  export.viewer.test(
    view(map),
    filename = "map_3d.html"
  )

})

# 3D map
test_that("Viewing a 3D map with sphere outlines", {

  map <- read.acmap(test_path("../testdata/testmap_h3subset3d.ace"))
  srShape(map) <- "CIRCLE"

  export.viewer.test(
    view(map),
    filename = "map_3d_sphere_outlines.html"
  )

})

# Exporting the viewer
test_that("Exporting a map viewer", {

  tmp <- tempfile(fileext = ".html")
  export_viewer(map, tmp)
  expect_true(file.exists(tmp))
  unlink(tmp)

})

# Overlaid points
test_that("Map with triangle points", {

  ag_coords1 <- cbind(1:5, 1)
  ag_coords2 <- cbind(1:5, 2)
  sr_coords  <- cbind(1:5, 3)
  testmap <- acmap(
    ag_coords = rbind(ag_coords1, ag_coords2),
    sr_coords = sr_coords
  )

  agShape(testmap)[1:5]   <- "BOX"
  srShape(testmap)        <- "TRIANGLE"
  agAspect(testmap)[1:5]  <- seq(from = 0.5, to = 1.5, length.out = 5)
  agAspect(testmap)[6:10] <- seq(from = 0.5, to = 1.5, length.out = 5)
  srAspect(testmap)       <- seq(from = 0.5, to = 1.5, length.out = 5)
  agFill(testmap)[1]      <- "#ff0000"

  export.viewer.test(
    view(testmap),
    filename = "map_with_triangles.html"
  )

})


# Adding a map legend
test_that("Adding a map legend", {

  legendmap <- setLegend(
    map,
    legend = c("Blue points", "Red points"),
    fill   = c("#0000ff", "#ff0000")
  )

  export.viewer.test(
    view(legendmap),
    filename = "map_with_legend.html"
  )

})


# Point styles
test_that("Viewing map point styles", {

  stylemap <- acmap(
    ag_coords = cbind(1:5, 0),
    sr_coords = cbind(1:5, 1)
  )

  agOutlineWidth(stylemap) <- 1:5
  srOutlineWidth(stylemap) <- 1:5

  export.viewer.test(
    view(stylemap),
    filename = "map_with_pointstyles.html"
  )

})


# Rotated map
test_that("Viewing map rotation", {

  map <- acmap(
    ag_coords = cbind(0, 1:5),
    sr_coords = cbind(1:5, 0)
  )

  maprot <- rotateMap(
    map, 45
  )

  export.viewer.test(
    view(maprot),
    "map45degreeclockwise.html"
  )

})


# Setting viewer options
test_that("Viewing map rotation", {

  map <- acmap(
    ag_coords = cbind(0, 1:5),
    sr_coords = cbind(1:5, 0)
  )

  export.viewer.test(
    view(
      map,
      options = list(
        viewer.controls = "shown",
        point.opacity = 0.2
      )
    ),
    "map_vieweroptions.html"
  )

})


# # Snapshot map
# test_that("Map snapshot", {
#
#   snapshotfile <- "~/Dropbox/LabBook/packages/Racmacs/tests/testoutput/viewer/mapsnapshot.png"
#   unlink(snapshotfile)
#
#   snapshotMap(
#     map,
#     filename = snapshotfile
#   )
#
#   expect_true(file.exists(snapshotfile))
#
# })
