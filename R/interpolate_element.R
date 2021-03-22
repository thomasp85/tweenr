interpolate_numeric_element <- function(data, group, frame, ease) {
  numeric_element_interpolator(data, group, frame, ease)
}
interpolate_logical_element <- function(data, group, frame, ease) {
  res <- numeric_element_interpolator(as.numeric(data), group, frame, ease)
  res[['data']] <- as.logical(round(res[['data']]))
  res
}
#' @importFrom grDevices col2rgb rgb
#' @importFrom farver convert_colour
interpolate_colour_element <- function(data, group, frame, ease) {
  col <- t(col2rgb(data, alpha = TRUE))
  data <- convert_colour(col[,1:3, drop = FALSE], from = 'rgb', to = 'lab')
  int_col <- colour_element_interpolator(cbind(data, col[,4]), group, frame, ease)
  int_col_convert <- convert_colour(as.matrix(int_col[, c('data1', 'data2', 'data3')]), from = 'lab', to = 'rgb')
  int_col_convert[int_col_convert > 255] <- 255
  int_col_convert[int_col_convert < 0] <- 0
  int_col$data4[int_col$data4 < 0] <- 0
  data.frame(
    data = rgb(int_col_convert[, 1], int_col_convert[, 2], int_col_convert[, 3], int_col$data4, maxColorValue = 255),
    group = int_col$group,
    frame = int_col$frame,
    stringsAsFactors = FALSE
  )
}
interpolate_constant_element <- function(data, group, frame, ease) {
  constant_element_interpolator(data, group, frame, ease)
}
interpolate_character_element <- function(data, group, frame, ease) {
  interpolate_constant_element(data, group, frame, ease)
}
interpolate_date_element <- function(data, group, frame, ease) {
  data <- as.numeric(data)
  res <- interpolate_numeric_element(data, group, frame, ease)
  res[['data']] <- as.Date(res[['data']], origin = BASEDATE)
  res
}
interpolate_datetime_element <- function(data, group, frame, ease) {
  if (inherits(data, 'POSIXlt')) {
    warning("POSIXlt converted to POSIXct")
    data <- as.POSIXct(data)
  }
  tz <- attr(data, 'tzone')
  data <- as.numeric(data)
  res <- interpolate_numeric_element(data, group, frame, ease)
  res[['data']] <-  as.POSIXct(res[['data']], origin = BASEDATETIME, tz = tz)
  res
}
interpolate_factor_element <- function(data, group, frame, ease) {
  all_levels <- levels(data)
  ord <- is.ordered(data)
  data <- as.character(data)
  res <- interpolate_character_element(data, group, frame, ease)
  res[['data']] <- if (ord) ordered(res[['data']], all_levels) else factor(res[['data']], all_levels)
  res
}
interpolate_list_element <- function(data, group, frame, ease) {
  new_data <- list_element_interpolator(data, group, frame, ease)
  attributes(new_data$data) <- attributes(data)
  new_data
}
interpolate_numlist_element <- function(data, group, frame, ease) {
  new_data <- numlist_element_interpolator(data, group, frame, ease)
  attributes(new_data$data) <- attributes(data)
  new_data
}
get_phase_element <- function(data, group, frame, ease) {
  phase_element_interpolator(data, group, frame, ease)
}
