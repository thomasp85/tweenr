interpolate_numeric_state <- function(data, states) {
  numeric_state_interpolator(data, states)
}
interpolate_logical_state <- function(data, states) {
  as.logical(round(numeric_state_interpolator(lapply(data, as.numeric), states)))
}
#' @importFrom grDevices col2rgb rgb
#' @importFrom farver convert_colour
interpolate_colour_state <- function(data, states) {
  data <- lapply(data, function(d){
    col <- t(col2rgb(d, alpha = TRUE))
    col_conv <- convert_colour(col[,1:3, drop = FALSE], from = 'rgb', to = 'lab')
    cbind(col_conv, col[,4])
  })
  int_col <- colour_state_interpolator(data, states)
  alpha <- int_col[,4]
  alpha[alpha > 255] <- 255
  alpha[alpha < 0] <- 0
  int_col <- convert_colour(int_col[, 1:3, drop = FALSE], from = 'lab', to = 'rgb')
  int_col[int_col > 255] <- 255
  int_col[int_col < 0] <- 0
  rgb(int_col[, 1], int_col[, 2], int_col[, 3], alpha, maxColorValue = 255)
}
interpolate_constant_state <- function(data, states) {
  constant_state_interpolator(data, states)
}
interpolate_character_state <- function(data, states) {
  interpolate_constant_state(data, states)
}
interpolate_date_state <- function(data, states) {
  data <- lapply(data, as.numeric)
  as.Date(interpolate_numeric_state(data, states), origin = BASEDATE)
}
interpolate_datetime_state <- function(data, states) {
  if (inherits(data[[1]], 'POSIXlt')) {
    warning("POSIXlt converted to POSIXct")
    data[[1]] <- as.POSIXct(data[[1]])
  }
  tz <- attr(data[[1]], 'tzone')
  data <- lapply(data, as.numeric)
  as.POSIXct(interpolate_numeric_state(data, states), origin = BASEDATETIME, tz = tz)
}
interpolate_factor_state <- function(data, states) {
  all_levels <- Reduce(union, lapply(data, levels))
  ord <- is.ordered(data[[1]])
  data <- lapply(data, as.character)
  data <- interpolate_character_state(data, states)
  if (ord) ordered(data, all_levels) else factor(data, all_levels)
}
interpolate_list_state <- function(data, states) {
  new_data <- list_state_interpolator(data, states)
  attributes(new_data) <- attributes(data)
  new_data
}
interpolate_numlist_state <- function(data, states) {
  new_data <- numlist_state_interpolator(lapply(data, lapply, as.numeric), states)
  attributes(new_data) <- attributes(data)
  new_data
}

get_phase_state <- function(data, states) {
  phase_state_interpolator(data, states)
}
