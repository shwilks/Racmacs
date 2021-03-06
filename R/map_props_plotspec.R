
# Function factory for plotspec getter functions
plotspec_getter <- function(pttype, fn) {
  eval(
    substitute(env = list(
      pttype = pttype,
      fn = fn
    ), expr = {
      if (pttype == "ag") {
        function(map) {
          check.acmap(map)
          sapply(map$antigens, function(ag) {
            fn(ag$plotspec)
          })
        }
      } else {
        function(map) {
          check.acmap(map)
          sapply(map$sera, function(sr) {
            fn(sr$plotspec)
          })
        }
      }
    })
  )
}

# Function factory for plotspec setter functions
plotspec_setter <- function(pttype, fn, checker_fn = NULL) {
  eval(
    substitute(env = list(
      pttype = pttype,
      fn = fn
    ), expr = {
      if (pttype == "ag") {
        function(map, value) {
          check.acmap(map)
          if (is.null(value)) {
            stop("Cannot set null value")
          }
          if (!is.null(checker_fn)) {
            checker_fn(value)
          }
          if (length(value) == 1) {
            value <- rep_len(value, numAntigens(map))
          } else if (length(value) != numAntigens(map)) {
            stop("Input does not match the number of antigens", call. = FALSE)
          }
          map$antigens <- lapply(seq_along(map$antigens), function(x) {
            map$antigens[[x]]$plotspec <- fn(map$antigens[[x]]$plotspec, value[x])
            map$antigens[[x]]
          })
          map
        }
      } else {
        function(map, value) {
          check.acmap(map)
          if (is.null(value)) {
            stop("Cannot set null value")
          }
          if (!is.null(checker_fn)) {
            checker_fn(value)
          }
          if (length(value) == 1) {
            value <- rep_len(value, numSera(map))
          } else if (length(value) != numSera(map)) {
            stop("Input does not match the number of sera", call. = FALSE)
          }
          map$sera <- lapply(seq_along(map$sera), function(x) {
            map$sera[[x]]$plotspec <- fn(map$sera[[x]]$plotspec, value[x])
            map$sera[[x]]
          })
          map
        }
      }
    })
  )
}


#' Getting and setting point plotting styles
#'
#' These functions get and set the styles to use for each point when plotting.
#'
#' @name ptStyles
#' @family {map point style functions}
#' @eval roxygen_tags(
#'   methods = c(
#'   "agShown", "srShown", "agShown<-", "srShown<-",
#'   "agSize", "srSize", "agSize<-", "srSize<-",
#'   "agFill", "srFill", "agFill<-", "srFill<-",
#'   "agOutline", "srOutline", "agOutline<-", "srOutline<-",
#'   "agOutlineWidth", "srOutlineWidth", "agOutlineWidth<-", "srOutlineWidth<-",
#'   "agRotation", "srRotation", "agRotation<-", "srRotation<-",
#'   "agAspect", "srAspect", "agAspect<-", "srAspect<-",
#'   "agShape", "srShape", "agShape<-", "srShape<-"
#'   ),
#'   args = c("map")
#' )
#'
agShown        <- plotspec_getter("ag", ac_plotspec_get_shown)
agSize         <- plotspec_getter("ag", ac_plotspec_get_size)
agFill         <- plotspec_getter("ag", ac_plotspec_get_fill)
agOutline      <- plotspec_getter("ag", ac_plotspec_get_outline)
agOutlineWidth <- plotspec_getter("ag", ac_plotspec_get_outline_width)
agRotation     <- plotspec_getter("ag", ac_plotspec_get_rotation)
agAspect       <- plotspec_getter("ag", ac_plotspec_get_aspect)
agShape        <- plotspec_getter("ag", ac_plotspec_get_shape)
srShown        <- plotspec_getter("sr", ac_plotspec_get_shown)
srSize         <- plotspec_getter("sr", ac_plotspec_get_size)
srFill         <- plotspec_getter("sr", ac_plotspec_get_fill)
srOutline      <- plotspec_getter("sr", ac_plotspec_get_outline)
srOutlineWidth <- plotspec_getter("sr", ac_plotspec_get_outline_width)
srRotation     <- plotspec_getter("sr", ac_plotspec_get_rotation)
srAspect       <- plotspec_getter("sr", ac_plotspec_get_aspect)
srShape        <- plotspec_getter("sr", ac_plotspec_get_shape)

`agShown<-`        <- plotspec_setter("ag", ac_plotspec_set_shown, check.logicalvector)
`agSize<-`         <- plotspec_setter("ag", ac_plotspec_set_size, check.numericvector)
`agFill_raw<-`     <- plotspec_setter("ag", ac_plotspec_set_fill, check.charactervector)
`agOutline_raw<-`  <- plotspec_setter("ag", ac_plotspec_set_outline, check.charactervector)
`agOutlineWidth<-` <- plotspec_setter("ag", ac_plotspec_set_outline_width, check.numericvector)
`agRotation<-`     <- plotspec_setter("ag", ac_plotspec_set_rotation, check.numericvector)
`agAspect<-`       <- plotspec_setter("ag", ac_plotspec_set_aspect, check.numericvector)
`agShape<-`        <- plotspec_setter("ag", ac_plotspec_set_shape, check.charactervector)
`srShown<-`        <- plotspec_setter("sr", ac_plotspec_set_shown, check.logicalvector)
`srSize<-`         <- plotspec_setter("sr", ac_plotspec_set_size, check.numericvector)
`srFill_raw<-`     <- plotspec_setter("sr", ac_plotspec_set_fill, check.charactervector)
`srOutline_raw<-`  <- plotspec_setter("sr", ac_plotspec_set_outline, check.charactervector)
`srOutlineWidth<-` <- plotspec_setter("sr", ac_plotspec_set_outline_width, check.numericvector)
`srRotation<-`     <- plotspec_setter("sr", ac_plotspec_set_rotation, check.numericvector)
`srAspect<-`       <- plotspec_setter("sr", ac_plotspec_set_aspect, check.numericvector)
`srShape<-`        <- plotspec_setter("sr", ac_plotspec_set_shape, check.charactervector)

# Extra functions that include a color validation step
validate_colors <- function(cols) {
  tryCatch(
    grDevices::col2rgb(cols),
    error = function(e) {
      stop(e$message, call. = FALSE)
    }
  )
}

`agFill<-` <- function(map, value) {
  validate_colors(value)
  `agFill_raw<-`(map, value)
}

`srFill<-` <- function(map, value) {
  validate_colors(value)
  `srFill_raw<-`(map, value)
}

`agOutline<-` <- function(map, value) {
  validate_colors(value)
  `agOutline_raw<-`(map, value)
}

`srOutline<-` <- function(map, value) {
  validate_colors(value)
  `srOutline_raw<-`(map, value)
}


#' Get and set point drawing order in map
#'
#' Point drawing order is a vector of indices defining the order in
#' which points should be draw when plotting or viewing a map. Points
#' are indexed in the same order as antigens then followed by
#' sera.
#'
#' @param map An acmap object
#' @param value The point drawing order
#'
#' @return Returns the map object with point drawing order information updated
#'
#' @family {map point style functions}
#'
#' @name ptDrawingOrder
#'

#' @rdname ptDrawingOrder
#' @export
ptDrawingOrder <- function(map) {
  drawing_order <- map$pt_drawing_order
  if (is.null(drawing_order)) drawing_order <- seq_len(numPoints(map))
  as.vector(drawing_order)
}

#' @rdname ptDrawingOrder
#' @export
`ptDrawingOrder<-` <- function(map, value) {
  check.numericvector(value)
  if (!isTRUE(all.equal(sort(value), seq_len(numPoints(map))))) {
    stop("drawing incorrectly specified", call. = FALSE)
  }
  map$pt_drawing_order <- value
  map
}
