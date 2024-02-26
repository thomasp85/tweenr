interpolate_numeric_at <- function(from, to, at, ease) {
  numeric_at_interpolator(as.numeric(from), as.numeric(to), as.numeric(at), as.character(ease))
}

interpolate_logical_at <- function(from, to, at, ease) {
  as.logical(round(interpolate_numeric_at(from, to, at, ease)))
}

#' @importFrom farver decode_colour encode_colour
interpolate_colour_at <- function(from, to, at, ease) {
  from <- decode_colour(from, alpha = TRUE, to = 'lab')
  to <- decode_colour(to, alpha = TRUE, to = 'lab')
  data <- colour_at_interpolator(from, to, as.numeric(at), as.character(ease))
  encode_colour(data[, 1:3, drop = FALSE], alpha = data[,4], from = 'lab')
}

interpolate_constant_at <- function(from, to, at, ease) {
  constant_at_interpolator(as.character(from), as.character(to), as.numeric(at), as.character(ease))
}

interpolate_character_at <- interpolate_constant_at

interpolate_date_at <- function(from, to, at, ease) {
  data <- interpolate_numeric_at(from, to, at, ease)
  as.Date(data, origin = BASEDATE)
}

interpolate_datetime_at <- function(from, to, at, ease) {
  if (inherits(from, 'POSIXlt')) {
    warning("POSIXlt converted to POSIXct")
    from <- as.POSIXct(from)
  }
  tz <- attr(from, 'tzone')
  data <- interpolate_numeric_at(from, to, at, ease)
  as.POSIXct(data, origin = BASEDATETIME, tz = tz)
}

interpolate_factor_at <- function(from, to, at, ease) {
  all_levels <- unique(c(levels(from), levels(to)))
  data <- interpolate_constant_at(from, to, at, ease)
  if (is.ordered(from)) ordered(data, all_levels) else factor(data, all_levels)
}

interpolate_list_at <- function(from, to, at, ease) {
  data <- list_at_interpolator(as.list(from), as.list(to), as.numeric(at), as.character(ease))
  attributes(data) <- attributes(from)
  data
}

interpolate_numlist_at <- function(from, to, at, ease) {
  data <- numlist_at_interpolator(lapply(from, as.numeric), lapply(to, as.numeric), as.numeric(at), as.character(ease))
  attributes(data) <- attributes(from)
  data
}

interpolate_numeric_at_t <- function(from, to, at, ease) {
  numeric_at_t_interpolator(as.numeric(from), as.numeric(to), as.numeric(at), as.character(ease))
}

interpolate_logical_at_t <- function(from, to, at, ease) {
  as.logical(round(interpolate_numeric_at_t(from, to, at, ease)))
}

#' @importFrom farver decode_colour encode_colour
interpolate_colour_at_t <- function(from, to, at, ease) {
  from <- decode_colour(from, alpha = TRUE, to = 'lab')
  to <- decode_colour(to, alpha = TRUE, to = 'lab')
  data <- colour_at_t_interpolator(from, to, as.numeric(at), as.character(ease))
  encode_colour(data[, 1:3, drop = FALSE], alpha = data[,4], from = 'lab')
}

interpolate_constant_at_t <- function(from, to, at, ease) {
  constant_at_t_interpolator(as.character(from), as.character(to), as.numeric(at), as.character(ease))
}

interpolate_character_at_t <- interpolate_constant_at_t

interpolate_date_at_t <- function(from, to, at, ease) {
  data <- interpolate_numeric_at(from, to, at, ease)
  as.Date(data, origin = BASEDATE)
}

interpolate_datetime_at_t <- function(from, to, at, ease) {
  if (inherits(from, 'POSIXlt')) {
    warning("POSIXlt converted to POSIXct")
    from <- as.POSIXct(from)
  }
  tz <- attr(from, 'tzone')
  data <- interpolate_numeric_at(from, to, at, ease)
  as.POSIXct(data, origin = BASEDATETIME, tz = tz)
}

interpolate_factor_at_t <- function(from, to, at, ease) {
  all_levels <- unique(c(levels(from), levels(to)))
  data <- interpolate_constant_at(from, to, at, ease)
  if (is.ordered(from)) ordered(data, all_levels) else factor(data, all_levels)
}

interpolate_list_at_t <- function(from, to, at, ease) {
  data <- list_at_t_interpolator(as.list(from), as.list(to), as.numeric(at), as.character(ease))
  attributes(data) <- attributes(from)
  data
}

interpolate_numlist_at_t <- function(from, to, at, ease) {
  data <- numlist_at_t_interpolator(lapply(from, as.numeric), lapply(to, as.numeric), as.numeric(at), as.character(ease))
  attributes(data) <- attributes(from)
  data
}
