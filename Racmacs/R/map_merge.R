
#' Merging maps
#'
#' Functions to merge together two tables or maps.
#'
#' @param map1 The first map
#' @param map2 The second map
#' @param passage_matching Passage matching
#' @param optimization_number_map1 Optimization number from the first map to merge
#' @param optimization_number_map2 Optimization number from the second map to merge
#' @param method Merging method
#' @param optimizations Number of optimization runs to perform (for 'incremental method')
#'
#' @details Maps can be merged in a number of ways depending upon the desired result.
#'
#' \subsection{Method 'table'}{
#' This merges the tables of the two maps but does not attempt to create any new optimizations.
#' }
#'
#' \subsection{Method 'incremental'}{
#' The default or specified optimization of the first map is copied to the resulting
#' map, optimizations of the second (merged in) map are ignored. Positions of
#' points that are in the merged-in map and not in the first map are
#' randomized and resulting layout relaxed.
#' }
#'
#' \subsection{Method 'overlay'}{
#' This takes the default or specified optimization from the second map and
#' realigns it to the default or specifed optimization of the first map using
#' procrustes, then points that are found just in the second map receive
#' coordinates from that realigned optimization. Points that are common for the
#' first and the second map placed to their middle positions between the first
#' map layout and realigned second map layout.
#' }
#'
#' \subsection{Method 'overlay-relax'}{
#' Overlay merge followed by relaxation of the resulting optimization.
#' }
#'
#' @return Returns the merged map data or merge report.
#'
#' @name merging_maps
#'
NULL

#' @rdname merging_maps
#' @export
mergeReport <- function(map1,
                        map2,
                        passage_matching = "auto"){

  # Convert the maps
  if(class(map1)[1] == "racmap") map1 <- as.cpp(map1)
  if(class(map2)[1] == "racmap") map2 <- as.cpp(map2)

  report <- acmacs.r::acmacs.merge_report(map1$chart, map2$chart)
  message(report)
  invisible(report)

}


#' @rdname merging_maps
#' @export
mergeMaps <- function(...,
                      method                  = "table",
                      passage_matching        = "auto",
                      minimum_column_basis    = "none",
                      number_of_optimizations = 100,
                      number_of_dimensions){

  # Check that optimizations isn't specified with one of the other methods
  if(!missing(number_of_optimizations) && method %in% c("incremental", "reoptimized-merge")){
    stop("Number of optimizations is only relevant for the merging method 'incremental'.")
  }

  # Create a list of maps
  maps <- list(...)

  # Keep a record of the main map class
  map1_class <- class(maps[[1]])

  # Clone the maps
  maps <- lapply(maps, cloneMap)

  # Keep only the required optimizations
  if(method %in% c("table", "reoptimized-merge")) {
    maps <- lapply(maps, removeOptimizations)
  } else if(method == "incremental-merge") {
    maps[[1]]            <- keepSingleOptimization(maps)
    maps[2:length(maps)] <- lapply(maps[2:length(maps)], removeOptimizations)
  } else {
    maps <- lapply(maps, function(map){
      if(numOptimizations(map) > 1) keepSingleOptimization(map)
      else                          map
    })
  }

  # Convert the maps to cpp
  maps <- lapply(maps, as.cpp)

  # Merge the charts
  merged_chart <- maps[[1]]$chart

  if(method == "table"){

    # Table merging
    for(x in 2:length(maps)){
      merged_chart <- acmacs.r::acmacs.merge(
        chart1 = merged_chart,
        chart2 = maps[[x]]$chart,
        match  = passage_matching,
        merge  = 1
      )
    }

  } else if(method == "reoptimized-merge"){

    # Reoptimized merging
    for(x in 2:length(maps)){
      merged_chart <- acmacs.r::acmacs.merge(
        chart1 = merged_chart,
        chart2 = maps[[x]]$chart,
        match  = passage_matching,
        merge  = 1
      )
    }

    merged_chart$relax_many(
      minimum_column_basis,
      number_of_dimensions,
      number_of_optimizations,
      FALSE
    )

  } else if(method == "incremental-merge"){

    # Incremental merging
    for(x in 2:length(maps)){
      merged_chart <- acmacs.r::acmacs.merge(
        chart1 = merged_chart,
        chart2 = maps[[x]]$chart,
        match  = passage_matching,
        merge  = 2
      )
      merged_chart$relax_incremental(number_of_optimizations, FALSE)
    }

  } else if(method == "frozen-overlay"
            || method == "relaxed-overlay"){

    # Frozen overlay
    if(length(maps) > 2){
      stop("A maximum of 2 maps can be merged with the frozen-overlay method at a time")
    }

    for(x in 2:length(maps)){
      if(numOptimizations(maps[[x]]) == 0){
        stop("Cannot perform a overlay merge because the second map does not have any optimizations")
      }
      merged_chart <- acmacs.r::acmacs.merge(
        chart1 = merged_chart,
        chart2 = maps[[x]]$chart,
        match  = passage_matching,
        merge  = 3
      )
    }

    # Frozen overlay with relaxation
    if(method == "relaxed-overlay"){
      merged_chart$projections[[1]]$relax()
    }

  } else if(method == "frozen-merge"){

    # Frozen merge
    if(length(maps) > 2){
      stop("A maximum of 2 maps can be merged with the frozen-merge method at a time")
    }

    for(x in 2:length(maps)){
      # if(maps[[x]]$chart$number_of_projections  == 0){
      #   maps[[x]]$chart$relax_many(
      #     merged_chart$projections[[1]]$minimum_column_basis,
      #     merged_chart$projections[[1]]$number_of_dimensions,
      #     number_of_optimizations,
      #     FALSE
      #   )
      # }
      merged_chart <- acmacs.r::acmacs.merge(
        chart1 = merged_chart,
        chart2 = maps[[x]]$chart,
        match  = passage_matching,
        merge  = 5
      )
    }

  } else {

    stop("'merge' must be one of 'table', 'reoptimized-merge', 'incremental-merge', 'frozen-overlay', 'relaxed-overlay', 'frozen-merge'.")

  }

  # Return a new map
  merged_map <- racchart.new(chart = merged_chart)
  if(method != "table")      selectedOptimization(merged_map) <- 1
  if("racmap" %in% map1_class) merged_map <- as.list(merged_map)
  merged_map

}



