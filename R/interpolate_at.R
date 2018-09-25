interpolate_numeric_at <- function(from, to, at, ease) {
  numeric_at_interpolator(from, to, at, ease)
}

interpolate_logical_at <- function(from, to, at, ease) {
  as.logical(round(numeric_at_interpolator(from, to, at, ease)))
}

interpolate_colour_at <- function(from, to, at, ease) {
  from <- t(col2rgb(from, alpha = TRUE))
  from[, 1:3] <- convert_colour(from[, 1:3, drop = FALSE], from = 'rgb', to = 'lab')
  to <- t(col2rgb(to, alpha = TRUE))
  to[, 1:3] <- convert_colour(to[, 1:3, drop = FALSE], from = 'rgb', to = 'lab')
  data <- colour_at_interpolator(from, to, at, ease)
  data[, 1:3] <- convert_colour(data[, 1:3, drop = FALSE], from = 'lab', to = 'rgb')
  data[data > 255] <- 255
  data[data < 0] <- 0
  rgb(data, alpha = data[, 4], maxColorValue = 255L)
}

interpolate_character_at <- function(from, to, at, ease) {
  constant_at_interpolator(from, to, at, ease)
}

interpolate_constant_at <- function(from, to, at, ease) {
  constant_at_interpolator(from, to, at, ease)
}

interpolate_date_at <- function(from, to, at, ease) {
  data <- numeric_at_interpolator(as.numeric(from), as.numeric(to), at, ease)
  as.Date(data, origin = BASEDATE)
}

interpolate_datetime_at <- function(from, to, at, ease) {
  if (inherits(from, 'POSIXlt')) {
    warning("POSIXlt converted to POSIXct")
  }
  data <- numeric_at_interpolator(as.numeric(from), as.numeric(to), at, ease)
  as.POSIXct(data, origin = BASEDATETIME)
}

interpolate_factor_at <- function(from, to, at, ease) {
  all_levels <- unique(c(levels(from), levels(to)))
  data <- constant_at_interpolator(as.character(from), as.character(to), at, ease)
  factor(data, all_levels)
}

interpolate_list_at <- function(from, to, at, ease) {
  data <- list_at_interpolator(from, to, at, ease)
  attributes(data) <- attributes(from)
  data
}

interpolate_numlist_at <- function(from, to, at, ease) {
  data <- numlist_at_interpolator(from, to, at, ease)
  attributes(data) <- attributes(from)
  data
}
