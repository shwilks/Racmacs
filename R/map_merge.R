
#' Merging maps
#'
#' Functions to merge together two tables or maps.
#'
#' @param maps List of acmaps to merge
#' @param method The merge method to use, see details.
#' @param number_of_dimensions For merging that generates new optimization runs,
#'   the number of dimensions.
#' @param number_of_optimizations For merging that generates new optimization
#'   runs, the number of optimization runs to do.
#' @param minimum_column_basis For merging that generates new optimization runs,
#'   the minimum column basis to use.
#' @param options For merging that generates new optimization runs, optimizer
#'   settings (see `RacOptimizer.options()`).
#'
#' @details Maps can be merged in a number of ways depending upon the desired
#'   result.
#'
#'   \subsection{Method 'table'}{ As you would expect, this merges the tables of
#'   the two maps but does not attempt to create any new optimizations and any
#'   existing optimizations are lost. }
#'
#'   \subsection{Method 'reoptimized-merge'}{ This merges the tables and then
#'   does a specified number of fresh optimizations from random starting
#'   coordinates, ignoring any pre-existing optimization runs. It's exactly the
#'   same as doing a 'table' merge and running `optimizeMap()` on the merged
#'   table. }
#'
#'   \subsection{Method 'incremental-merge'}{ This takes the currently selected
#'   optimization in the first map and then merges in the additional maps in
#'   turn. Each time any points not already found in the first map (or the last
#'   map in the incremental merge chain) are randomised and everything is
#'   relaxed, this is repeated the specified number of times and the process is
#'   repeated. }
#'
#'   \subsection{Method 'frozen-overlay'}{ This fixes the positions of points in
#'   each map and tries to best match them simply through re-orientation. Once
#'   the best re-orientation is found, points that are in common between the
#'   maps are moved to the average position. }
#'
#'   \subsection{Method 'relaxed-overlay'}{ This is the same as the
#'   frozen-overlay but points in the resulting map are then allowed to relax. }
#'
#'   \subsection{Method 'frozen-merge'}{ In this version, positions of all
#'   points in the first map are fixed and remain fixed, so the original map
#'   does not change. The second map is then realigned to the first as closely
#'   as possible and then all the new points appearing in the second map are
#'   allowed to relax into their new positions. This is a way to merge in new
#'   antigens and sera into a map without affecting the first one at all (and
#'   was first implemented in lisp). }
#'
#' @return Returns the merged map object
#'
#' @family {map merging functions}
#' @export
mergeMaps <- function(
  maps,
  method = "table",
  number_of_dimensions,
  number_of_optimizations,
  minimum_column_basis = "none",
  options = list()
  ) {

  # Check input
  if (!is.list(maps)) stop("Input must be a list of acmap objects", call. = FALSE)
  lapply(maps, check.acmap)

  # Set options for any relaxation or optimizations
  options <- do.call(RacOptimizer.options, options)

  # Apply the relevant merge method
  switch(
    method,
    # Table merge
    `table` = {
      ac_merge_tables(
        maps = maps
      )
    },
    # Re-optimized merge
    `reoptimized-merge` = {
      ac_merge_reoptimized(
        maps = maps,
        num_dims = number_of_dimensions,
        num_optimizations = number_of_optimizations,
        options = options
      )
    },
    # Incremental merge
    `incremental-merge` = {
      ac_merge_incremental(
        maps = maps,
        num_dims = number_of_dimensions,
        num_optimizations = number_of_optimizations,
        min_colbasis = minimum_column_basis,
        options = options
      )
    },
    # Frozen overlay merge
    `frozen-overlay` = {
      ac_merge_frozen_overlay(
        maps = maps
      )
    },
    # Relaxed overlay merge
    `relaxed-overlay` = {
      ac_merge_relaxed_overlay(
        maps = maps,
        options = options
      )
    },
    # Frozen overlay merge
    `frozen-merge` = {
      ac_merge_frozen_merge(
        maps = maps,
        options = options
      )
    },
    # Other merge
    stop(sprintf("Merge type '%s' not recognised", method), call. = FALSE)
  )

}


#' Return a merge report
#'
#' Prints a raw text merge report from merging two map tables.
#'
#' @param maps List of acmaps to merge
#'
#' @family {map merging functions}
#' @export
mergeReport <- function(
  maps
  ) {

  lapply(maps, check.acmap)
  stop("mergeReport not yet implemented")

}
