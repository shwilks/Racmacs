
#' Return calculated table distances for an acmap
#'
#' Takes the acmap object and, assuming the column bases associated with the
#' currently selected or specifed optimization, returns the table distances
#' calculated from the titer data. For more information on column bases and
#' their role in antigenic cartography see
#' `vignette("intro-to-antigenic-cartography")`
#'
#' @param map The acmap data object
#' @param optimization_number The optimization number
#'
#' @return Returns a matrix of numeric table distances
#' @export
#'
#' @family {map diagnostic functions}
#'   {functions relating to map stress calculation}
#'
tableDistances <- function(
  map,
  optimization_number = 1
  ) {

  if (numOptimizations(map) == 0) {
    stop("This map has no optimizations for which to calculate table distances")
  }
  numeric_dists <- ac_numeric_table_distances(
    titer_table = titerTable(map),
    min_col_basis = minColBasis(map, optimization_number),
    fixed_col_bases = fixedColBases(map, optimization_number),
    ag_reactivity_adjustments = agReactivityAdjustments(map, optimization_number)
  )
  numeric_dists[titertypesTable(map) == 0] <- "*"
  numeric_dists[titertypesTable(map) == 2] <- paste0(">", numeric_dists[titertypesTable(map) == 2])
  numeric_dists[titertypesTable(map) == 3] <- "NA"
  numeric_dists

}

# Backend function to get numeric form of table distances
numeric_min_tabledists <- function(tabledists) {

  thresholded <- substr(tabledists, 1, 1) == ">"
  tabledists[thresholded] <- substr(tabledists[thresholded], 2, nchar(tabledists[thresholded]))
  tabledists[tabledists == "*"] <- NA
  mode(tabledists) <- "numeric"
  tabledists[thresholded] <- tabledists[thresholded] + 1
  tabledists

}


#' Calculate column bases for a titer table
#'
#' For more information on column bases, what they mean and how they are
#' calculated see `vignette("intro-to-antigenic-cartography")`
#'
#' @param titer_table The titer table
#' @param minimum_column_basis The minimum column basis to assume
#' @param fixed_column_bases Fixed column bases to apply
#' @param ag_reactivity_adjustments Reactivity adjustments to apply on a per-antigen basis
#'
#' @return Returns a numeric vector of the log-converted column bases for the
#'   table
#'
#' @family {map diagnostic functions}
#'   {functions relating to map stress calculation}
#' @export
#'
tableColbases <- function(
  titer_table,
  minimum_column_basis = "none",
  fixed_column_bases = rep(NA, ncol(titer_table)),
  ag_reactivity_adjustments = rep(0, nrow(titer_table))
  ) {

  check.charactermatrix(titer_table)
  check.string(minimum_column_basis)
  fixed_column_bases <- check.numericvector(fixed_column_bases)
  ag_reactivity_adjustments <- check.numericvector(ag_reactivity_adjustments)

  ac_table_colbases(
    titer_table = format_titers(titer_table),
    min_col_basis = minimum_column_basis,
    fixed_col_bases = fixed_column_bases,
    ag_reactivity_adjustments = ag_reactivity_adjustments
  )

}


#' Return calculated map distances for an acmap
#'
#' Takes the acmap object and calculates euclidean distances between antigens
#' and sera for the currently selected or specified optimization.
#'
#' @param map The acmap data object
#' @param optimization_number The optimization number
#'
#' @return Returns a matrix of map distances with antigens as rows and sera as
#'   columns.
#' @export
#'
#' @family {map diagnostic functions}
#'   {functions relating to map stress calculation}
#'
mapDistances <- function(
  map,
  optimization_number = 1
  ) {

  if (numOptimizations(map) == 0) {
    stop("This map has no optimizations for which to calculate map distances")
  }

  ac_coordDistMatrix(
    agCoords(map, optimization_number),
    srCoords(map, optimization_number)
  )

}


#' Get the log titers from an acmap
#'
#' Converts titers to the log scale via via the transformation
#' $log2(x/10)$, lessthan values are reduced by 1 on the log scale and greater
#' than values are increased by 1, hence <10 => -1 and >1280 => 8
#'
#' @param map The acmap object
#'
#' @return Returns a matrix of titers converted to the log scale
#' @export
#'
#' @family {map diagnostic functions}
#'   {functions relating to map stress calculation}
#'
logtiterTable <- function(map) {

  matrix(
    log_titers(titerTable(map)),
    numAntigens(map),
    numSera(map)
  )

}


#' Get a stress table from an acmap
#'
#' @param map The acmap object
#' @param optimization_number The optimization number for which to calculate
#'   stresses
#'
#' @return Returns a matrix of stresses, showing how much each antigen and sera
#'   measurement contributes to stress in the selected or specified
#'   optimization.
#' @export
#'
#' @family {map diagnostic functions}
#'   {functions relating to map stress calculation}
#'
stressTable <- function(
  map,
  optimization_number = 1
  ) {

  if (numOptimizations(map) == 0) {
    stop(strwrap(
      "This map has no optimizations for which
      to calculate a stress table"
    ))
  }

  ac_point_stresses(
    titerTable(map),
    minColBasis(map, optimization_number),
    fixedColBases(map, optimization_number),
    agReactivityAdjustments(map, optimization_number),
    mapDistances(map, optimization_number)
  )

}


#' Get a table of residuals from an acmap
#'
#' This is the difference between the table distance and the map distance
#'
#' @param map The acmap object
#' @param exclude_nd Should values associated with non-detectable measurements
#'   like <10 be set to NA
#' @param optimization_number The optimization number
#'
#' @return Returns a matrix of residuals, showing the residual error between
#'   map distance and table distance for each antigen-sera pair.
#' @export
#'
#' @family {map diagnostic functions}
#'   {functions relating to map stress calculation}
#'
mapResiduals <- function(
  map,
  exclude_nd          = FALSE,
  optimization_number = 1
) {

  if (numOptimizations(map) == 0) {
    stop(strwrap(
      "This map has no optimizations for which
      to calculate a residual table"
    ))
  }

  residual_matrix <- ac_point_residuals(
    titerTable(map),
    minColBasis(map, optimization_number),
    fixedColBases(map, optimization_number),
    agReactivityAdjustments(map, optimization_number),
    mapDistances(map, optimization_number)
  )

  if (exclude_nd) {
    titertypes <- titertypesTable(map)
    residual_matrix[titertypes != 1] <- NA
  }

  residual_matrix

}


#' Recalculate the stress associated with an acmap optimization
#'
#' Recalculates the stress associated with the currently selected or
#' user-specifed optimization.
#'
#' @param map The acmap data object
#' @param optimization_number The optimization number
#'
#' @return Returns the recalculated map stress for a given optimization
#'
#' @family {map diagnostic functions}
#'   {functions relating to map stress calculation}
#' @seealso See `pointStress()` for getting the stress of individual points.
#' @export
recalculateStress <- function(
  map,
  optimization_number = 1
  ) {

  check.acmap(map)
  check.optnum(map, optimization_number)

  ac_coords_stress(
    titers = titerTable(map),
    min_colbasis = minColBasis(map, optimization_number),
    fixed_colbases = fixedColBases(map, optimization_number),
    ag_reactivity_adjustments = agReactivityAdjustments(map, optimization_number),
    ag_coords = agBaseCoords(map, optimization_number),
    sr_coords = srBaseCoords(map, optimization_number)
  )

}


#' Get individual point stress
#'
#' Functions to get stress associated with individual points in a map.
#'
#' @param map The acmap data object
#' @param optimization_number The optimization number
#' @param antigens Which antigens to check stress for, specified by index or
#'   name (defaults to all antigens).
#' @param sera Which sera to check stress for, specified by index or name
#'   (defaults to all sera).
#' @param exclude_nd Should non-detectable values (e.g. <10) be excluded when
#'   calculating point stress?
#'
#' @seealso See `mapStress()` for getting the total map stress directly.
#' @family {map diagnostic functions}
#'   {functions relating to map stress calculation}
#' @name pointStress
#'

#' @rdname pointStress
#' @export
agStress <- function(
  map,
  antigens            = TRUE,
  optimization_number = 1
) {

  # Convert to indices
  antigens <- get_ag_indices(antigens, map, warnings = TRUE)

  # Calculate the stress
  stress_table <- stressTable(map, optimization_number)
  rowSums(stress_table[antigens, ])

}

#' @rdname pointStress
#' @export
srStress <- function(
  map,
  sera                = TRUE,
  optimization_number = 1
) {

  # Convert to indices
  sera <- get_sr_indices(sera, map, warnings = TRUE)

  # Calculate the stress
  stress_table <- stressTable(map, optimization_number)
  colSums(stress_table[, sera])

}

#' @rdname pointStress
#' @export
srStressPerTiter <- function(
  map,
  sera                = TRUE,
  optimization_number = 1,
  exclude_nd          = FALSE
) {

  # Convert to indices
  sera <- get_sr_indices(sera, map, warnings = TRUE)

  # Get map residuals omitting anything that's not a measurable value
  map_residuals <- mapResiduals(
    map                 = map,
    optimization_number = optimization_number,
    exclude_nd          = exclude_nd
  )

  # Calculate the serum stress per titer
  vapply(sera, function(serum) {

    sr_residuals <- map_residuals[, serum]
    sr_residuals <- sr_residuals[!is.na(sr_residuals)]
    sum(sr_residuals^2) / length(sr_residuals)

  }, numeric(1))

}


#' @rdname pointStress
#' @export
agStressPerTiter <- function(
  map,
  antigens            = TRUE,
  optimization_number = 1,
  exclude_nd          = FALSE
) {

  # Convert to indices
  antigens <- get_ag_indices(antigens, map, warnings = TRUE)

  # Get map residuals omitting anything that's not a measurable value
  map_residuals <- mapResiduals(
    map                 = map,
    optimization_number = optimization_number,
    exclude_nd          = exclude_nd
  )

  # Calculate the antigen stress per titer
  vapply(antigens, function(antigen) {

    ag_residuals <- map_residuals[antigen, ]
    ag_residuals <- ag_residuals[!is.na(ag_residuals)]
    sum(ag_residuals^2) / length(ag_residuals)

  }, numeric(1))

}


# Get a matrix of numeric titer types, where < or > is removed
# 40 => 40, <10 => 10, >1280 => 1280
numerictiterTable <- function(map) {

  matrix(
    numeric_titers(titerTable(map)),
    numAntigens(map),
    numSera(map)
  )

}

# Get a matrix of integer types representing the titer types
# 0: unmeasured, 1: measurable, 2: lessthan, 3: morethan
titertypesTable <- function(map) {

  matrix(
    titer_types_int(titerTable(map)),
    numAntigens(map),
    numSera(map)
  )

}
