interpolate_numeric_along <- function(data, group, frame, nframes, ease, history, keep_last) {
  numeric_along_interpolator(data, group, frame, history, keep_last, nframes, ease)
}
interpolate_logical_along <- function(data, group, frame, nframes, ease, history, keep_last) {
  res <- numeric_along_interpolator(data, group, frame, history, keep_last, nframes, ease)
  res[['data']] <- as.logical(round(res[['data']]))
  res
}
#' @importFrom grDevices col2rgb rgb
#' @importFrom farver convert_colour
interpolate_colour_along <- function(data, group, frame, nframes, ease, history, keep_last) {
  col <- t(col2rgb(data, alpha = TRUE))
  data <- convert_colour(col[,1:3, drop = FALSE], from = 'rgb', to = 'lab')
  int_col <- colour_along_interpolator(cbind(data, col[,4]), group, frame, history, keep_last, nframes, ease)
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
interpolate_constant_along <- function(data, group, frame, nframes, ease, history, keep_last) {
  constant_along_interpolator(data, group, frame, history, keep_last, nframes, ease)
}
interpolate_character_along <- function(data, group, frame, nframes, ease, history, keep_last) {
  constant_along_interpolator(data, group, frame, history, keep_last, nframes, ease)
}
interpolate_date_along <- function(data, group, frame, nframes, ease, history, keep_last) {
  data <- as.numeric(data)
  res <- interpolate_numeric_along(data, group, frame, nframes, ease, history, keep_last)
  res[['data']] <- as.Date(res[['data']], origin = BASEDATE)
  res
}
interpolate_datetime_along <- function(data, group, frame, nframes, ease, history, keep_last) {
  if (inherits(data, 'POSIXlt')) {
    warning("POSIXlt converted to POSIXct")
    data <- as.POSIXct(data)
  }
  tz <- attr(data, 'tzone')
  data <- as.numeric(data)
  res <- interpolate_numeric_along(data, group, frame, nframes, ease, history, keep_last)
  res[['data']] <-  as.POSIXct(res[['data']], origin = BASEDATETIME, tz = tz)
  res
}
interpolate_factor_along <- function(data, group, frame, nframes, ease, history, keep_last) {
  all_levels <- levels(data)
  ord <- is.ordered(data)
  data <- as.character(data)
  res <- interpolate_character_along(data, group, frame, nframes, ease, history, keep_last)
  res[['data']] <- if (ord) ordered(res[['data']], all_levels) else factor(res[['data']], all_levels)
  res
}
interpolate_list_along <- function(data, group, frame, nframes, ease, history, keep_last) {
  new_data <- list_along_interpolator(data, group, frame, history, keep_last, nframes, ease)
  attributes(new_data$data) <- attributes(data)
  new_data
}
interpolate_numlist_along <- function(data, group, frame, nframes, ease, history, keep_last) {
  new_data <- numlist_along_interpolator(data, group, frame, history, keep_last, nframes, ease)
  attributes(new_data$data) <- attributes(data)
  new_data
}
get_phase_along <- function(group, frame, nframes, history, keep_last) {
  phase_along_interpolator(group, frame, history, keep_last, nframes)
}
