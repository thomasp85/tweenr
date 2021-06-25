interpolate_numeric_fill <- function(data, ease) {
  numeric_fill_interpolator(as.numeric(data), as.character(ease))
}

interpolate_logical_fill <- function(data, ease) {
  as.logical(round(numeric_fill_interpolator(data, ease)))
}

#' @importFrom farver decode_colour encode_colour
interpolate_colour_fill <- function(data, ease) {
  data <- decode_colour(data, alpha = TRUE, to = 'lab')
  data <- colour_fill_interpolator(data, as.character(ease))
  encode_colour(data[, 1:3, drop = FALSE], alpha = data[,4], from = 'lab')
}

interpolate_constant_fill <- function(data, ease) {
  constant_fill_interpolator(as.character(data), as.character(ease))
}

interpolate_character_fill <- interpolate_constant_fill

interpolate_date_fill <- function(data, ease) {
  as.Date(interpolate_numeric_fill(data, ease), origin = BASEDATE)
}

interpolate_datetime_fill <- function(data, ease) {
  if (inherits(data, 'POSIXlt')) {
    warning("POSIXlt converted to POSIXct")
    data <- as.POSIXct(data)
  }
  as.POSIXct(interpolate_numeric_fill(data, ease), origin = BASEDATETIME, tz = attr(data, 'tzone'))
}

interpolate_factor_fill <- function(data, ease) {
  all_levels <- levels(data)
  ord <- is.ordered(data)
  data <- interpolate_character_fill(data, ease)
  if (ord) ordered(data, all_levels) else factor(data, all_levels)
}

interpolate_list_fill <- function(data, ease) {
  new_data <- list_fill_interpolator(as.list(data), as.character(ease))
  attributes(new_data) <- attributes(data)
  new_data
}

interpolate_numlist_fill <- function(data, ease) {
  new_data <- numlist_fill_interpolator(lapply(data, as.numeric), as.character(ease))
  attributes(new_data) <- attributes(data)
  new_data
}
