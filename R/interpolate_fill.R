interpolate_numeric_fill <- function(data, ease) {
  numeric_fill_interpolator(data, ease)
}
interpolate_logical_fill <- function(data, ease) {
  as.logical(round(numeric_fill_interpolator(as.numeric(data), ease)))
}
#' @importFrom grDevices col2rgb rgb
#' @importFrom farver convert_colour
interpolate_colour_fill <- function(data, ease) {
  NA_col <- is.na(data)
  data <- t(col2rgb(data, alpha = TRUE))
  data[, 1:3] <- convert_colour(data[, 1:3, drop = FALSE], from = 'rgb', to = 'lab')
  data[NA_col,] <- NA
  data <- colour_fill_interpolator(data, ease)
  NA_col <- is.na(data[, 1])
  data[NA_col, 1:3] <- convert_colour(data[NA_col, 1:3, drop = FALSE], from = 'lab', to = 'rgb')
  data[data > 255] <- 255
  data[data < 0] <- 0
  all_data <- rep(NA, nrow(data))
  all_data[!NA_col] <- rgb(data[!NA_col, , drop = FALSE], alpha = data[!NA_col, 4], maxColorValue = 255L)
  all_data
}
interpolate_constant_fill <- function(data, ease) {
  constant_fill_interpolator(data, ease)
}
interpolate_character_fill <- function(data, ease) {
  interpolate_constant_fill(data, ease)
}
interpolate_date_fill <- function(data, ease) {
  data <- lapply(data, as.numeric)
  as.Date(interpolate_numeric_fill(data, ease), origin = BASEDATE)
}
interpolate_datetime_fill <- function(data, ease) {
  if (inherits(data, 'POSIXlt')) {
    warning("POSIXlt converted to POSIXct")
  }
  as.POSIXct(interpolate_numeric_fill(as.numeric(data), ease), origin = BASEDATETIME)
}
interpolate_factor_fill <- function(data, ease) {
  all_levels <- levels(data)
  factor(interpolate_character_fill(as.character(data), ease), all_levels)
}
interpolate_list_fill <- function(data, ease) {
  new_data <- list_fill_interpolator(data, ease)
  attributes(new_data) <- attributes(data)
  new_data
}
interpolate_numlist_fill <- function(data, ease) {
  new_data <- numlist_fill_interpolator(lapply(data, as.numeric), ease)
  attributes(new_data) <- attributes(data)
  new_data
}
