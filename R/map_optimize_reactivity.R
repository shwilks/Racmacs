
#' Optimize antigen reactivity adjustments
#'
#' @param map The acmap object
#' @param optimization_number The optimization number for which to optimize
#'   antigen reactivity adjustments
#' @param reactivity_stress_weighting The weighting to apply when calculating
#'   how much antigen reactivity changes should additionally contribute to
#'   stress in the optimization regime (see details).
#' @param fixed_ag_reactivities A vector of fixed antigen reactivities,
#'   use NA values to distinguish the positions you would still like to
#'   be optimized.
#' @param start_pars A vector of starting parameters to use for the optimizer,
#'   you can still supply starting parameters for antigens listed in
#'   `fixed_ag_reactivities` but they will be ignored.
#' @param options A named list of additional options to pass to
#'   `RacOptimizer.options()`
#'
#' @return The axmap object is returned with antigen reactivity adjustments
#'   set to the value calculated in the optimizer. This can be queried with
#'   `agReactivityAdjustments()`.
#'
#' @export
#'
optimizeAgReactivity <- function(
  map,
  optimization_number = 1,
  reactivity_stress_weighting = 1,
  fixed_ag_reactivities = rep(NA, numAntigens(map)),
  start_pars = rep(0, numAntigens(map)),
  options = list()
  ) {

  # Check inputs and set defaults
  check.acmap(map)
  check.optnum(map, optimization_number)
  fixed_ag_reactivities <- check.numericvector(fixed_ag_reactivities)

  if (length(fixed_ag_reactivities) != numAntigens(map)) {
    stop("fixed_ag_reactivities does not match the number of antigens", call. = FALSE)
  }
  if (length(start_pars) != numAntigens(map)) {
    stop("start_pars does not match the number of antigens", call. = FALSE)
  }

  # Perform the optimization
  result <- stats::optim(
    par = start_pars[is.na(fixed_ag_reactivities)],
    fn = ac_reactivity_adjustment_stress,
    method = "L-BFGS-B",
    fixed_ag_reactivities = fixed_ag_reactivities,
    minimum_column_basis = minColBasis(map, optimization_number),
    fixed_column_bases = fixedColBases(map, optimization_number),
    titertable = titerTable(map),
    ag_coords = agBaseCoords(map, optimization_number),
    sr_coords = srBaseCoords(map, optimization_number),
    options = do.call(RacOptimizer.options, options),
    fixed_antigens = integer(),
    fixed_sera = integer(),
    titer_weights = matrix(1, numAntigens(map), numSera(map)),
    reactivity_stress_weighting = reactivity_stress_weighting
  )

  # Apply the reactivity adjustments
  optimized_reactivities <- fixed_ag_reactivities
  optimized_reactivities[is.na(optimized_reactivities)] <- result$par
  agReactivityAdjustments(map, optimization_number) <- optimized_reactivities

  # Relax and return the map
  relaxMap(map, optimization_number)

}
