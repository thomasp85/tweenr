interpolate_numeric_element_at <- function(data, group, time, at, ease) {
  numeric_element_at_interpolator(as.numeric(data), as.integer(group), as.numeric(time), as.numeric(at), as.character(ease))
}

interpolate_logical_element_at <- function(data, group, time, at, ease) {
  as.logical(interpolate_numeric_element_at(data, group, time, at, ease))
}

#' @importFrom farver decode_colour encode_colour
interpolate_colour_element_at <- function(data, group, time, at, ease) {
  data <- decode_colour(data, alpha = TRUE, to = 'lab')
  col <- colour_element_at_interpolator(data, as.integer(group), as.numeric(time), as.numeric(at), as.character(ease))
  encode_colour(col[, 1:3, drop = FALSE], alpha = col[,4], from = 'lab')
}

interpolate_constant_element_at <- function(data, group, time, at, ease) {
  constant_element_at_interpolator(as.character(data), as.integer(group), as.numeric(time), as.numeric(at), as.character(ease))
}

interpolate_character_element_at <- interpolate_constant_element_at

interpolate_date_element_at <- function(data, group, time, at, ease) {
  res <- interpolate_numeric_element_at(data, group, time, at, ease)
  as.Date(res, origin = BASEDATE)
}

interpolate_datetime_element_at <- function(data, group, time, at, ease) {
  if (inherits(data, 'POSIXlt')) {
    warning("POSIXlt converted to POSIXct")
    data <- as.POSIXct(data)
  }
  tz <- attr(data, 'tzone')
  res <- interpolate_numeric_element_at(data, group, time, at, ease)
  as.POSIXct(res, origin = BASEDATETIME, tz = tz)
}

interpolate_factor_element_at <- function(data, group, time, at, ease) {
  all_levels <- levels(data)
  ord <- is.ordered(data)
  res <- interpolate_character_element_at(data, group, time, at, ease)
  if (ord) ordered(res[['data']], all_levels) else factor(res[['data']], all_levels)
}

interpolate_list_element_at <- function(data, group, time, at, ease) {
  new_data <- list_element_at_interpolator(as.list(data), as.integer(group), as.numeric(time), as.numeric(at), as.character(ease))
  attributes(new_data) <- attributes(data)
  new_data
}

interpolate_numlist_element_at <- function(data, group, time, at, ease) {
  new_data <- numlist_element_at_interpolator(lapply(data, as.numeric), as.integer(group), as.numeric(time), as.numeric(at), as.character(ease))
  attributes(new_data) <- attributes(data)
  new_data
}

get_phase_element_at <- function(data, group, time, at, ease) {
  phase_element_at_interpolator(as.character(data), as.integer(group), as.numeric(time), as.numeric(at), as.character(ease))
}
