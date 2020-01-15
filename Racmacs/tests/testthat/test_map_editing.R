
library(Racmacs)
library(testthat)
context("Test editing of map data")

for(maptype in c("racmap", "racchart")){

  if(maptype == "racmap")   map <- read.acmap(test_path("../testdata/testmap.ace"))
  if(maptype == "racchart") map <- read.acmap.cpp(test_path("../testdata/testmap.ace"))

  test_that(paste("Edit antigen names",maptype), {

    updatedMap <- edit_agNames(map       = map,
                               old_names = agNames(map)[c(2, 4)],
                               new_names = c("Test 1", "Test 2"))

    # Update names
    expect_equal(object = agNames(updatedMap)[c(2, 4)],
                           expected = c("Test 1", "Test 2"))

    expect_equal(object = agNames(updatedMap)[-c(2, 4)],
                           expected = agNames(map)[-c(2, 4)])


    # Update table
    # expect_equal(object = rownames(titerTable(updatedMap))[c(2, 4)],
    #                        expected = c("Test 1", "Test 2"))

    expect_equal(object = rownames(titerTable(updatedMap))[-c(2, 4)],
                           expected = rownames(titerTable(map))[-c(2, 4)])


    # Update coordinates
    # expect_equal(object = rownames(agCoords(updatedMap))[c(2, 4)],
    #                        expected = c("Test 1", "Test 2"))

    expect_equal(object = rownames(agCoords(updatedMap))[-c(2, 4)],
                           expected = rownames(agCoords(map))[-c(2, 4)])

    # Expect warning if some names are unmatched
    expect_warning(
      edit_agNames(map       = map,
                   old_names = c(agNames(map)[c(2, 4)], "x", "y"),
                   new_names = c("Test 1", "Test 2", "Test 3", "Test 4"))
    )

    # Expect error if length of old and new names don't match
    expect_error(
      edit_agNames(map       = map,
                   old_names = c(agNames(map)[c(2, 4)]),
                   new_names = c("Test 1", "Test 2", "Test 3", "Test 4"))
    )


  })




  test_that(paste("Edit sera names",maptype), {

    updatedMap <- edit_srNames(map       = map,
                               old_names = srNames(map)[c(2, 4)],
                               new_names = c("Test 1", "Test 2"))

    # Update names
    expect_equal(object = srNames(updatedMap)[c(2, 4)],
                           expected = c("Test 1", "Test 2"))

    expect_equal(object = srNames(updatedMap)[-c(2, 4)],
                           expected = srNames(map)[-c(2, 4)])


    # Update table
    # expect_equal(object = colnames(titerTable(updatedMap))[c(2, 4)],
    #                        expected = c("Test 1", "Test 2"))

    expect_equal(object = colnames(titerTable(updatedMap))[-c(2, 4)],
                           expected = colnames(map$table)[-c(2, 4)])


    # Update coordinates
    # expect_equal(object = rownames(srCoords(updatedMap))[c(2, 4)],
    #                        expected = c("Test 1", "Test 2"))

    expect_equal(object = rownames(srCoords(updatedMap))[-c(2, 4)],
                           expected = rownames(srCoords(map))[-c(2, 4)])

    # Expect warning if some names are unmatched
    expect_warning(
      edit_srNames(map       = map,
                   old_names = c(srNames(map)[c(2, 4)], "x", "y"),
                   new_names = c("Test 1", "Test 2", "Test 3", "Test 4"))
    )

    # Expect error if length of old and new names don't match
    expect_error(
      edit_srNames(map       = map,
                   old_names = c(srNames(map)[c(2, 4)]),
                   new_names = c("Test 1", "Test 2", "Test 3", "Test 4"))
    )

  })

}
