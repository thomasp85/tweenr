interpolate_numeric_along <- function(data, group, frame, frames, ease, history, keep_last) {
  numeric_along_interpolator(as.numeric(data), as.integer(group), as.numeric(frame), as.logical(history), as.logical(keep_last), as.numeric(frames), as.character(ease))
}

interpolate_logical_along <- function(data, group, frame, frames, ease, history, keep_last) {
  res <- interpolate_numeric_along(data, group, frame, history, keep_last, frames, ease)
  res[['data']] <- as.logical(round(res[['data']]))
  res
}

#' @importFrom farver decode_colour encode_colour
interpolate_colour_along <- function(data, group, frame, frames, ease, history, keep_last) {
  data <- decode_colour(data, alpha = TRUE, to = 'lab')
  col <- colour_along_interpolator(data, as.integer(group), as.numeric(frame), as.logical(history), as.logical(keep_last), as.numeric(frames), as.character(ease))
  data.frame(
    data = encode_colour(col[, 1:3, drop = FALSE], alpha = col[,4], from = 'lab'),
    group = col$group,
    frame = col$frame,
    stringsAsFactors = FALSE
  )
}

interpolate_constant_along <- function(data, group, frame, frames, ease, history, keep_last) {
  constant_along_interpolator(as.character(data), as.integer(group), as.numeric(frame), as.logical(history), as.logical(keep_last), as.numeric(frames), as.character(ease))
}

interpolate_character_along <- interpolate_constant_along

interpolate_date_along <- function(data, group, frame, frames, ease, history, keep_last) {
  res <- interpolate_numeric_along(data, group, frame, frames, ease, history, keep_last)
  res[['data']] <- as.Date(res[['data']], origin = BASEDATE)
  res
}
interpolate_datetime_along <- function(data, group, frame, frames, ease, history, keep_last) {
  if (inherits(data, 'POSIXlt')) {
    warning("POSIXlt converted to POSIXct")
    data <- as.POSIXct(data)
  }
  tz <- attr(data, 'tzone')
  res <- interpolate_numeric_along(data, group, frame, frames, ease, history, keep_last)
  res[['data']] <-  as.POSIXct(res[['data']], origin = BASEDATETIME, tz = tz)
  res
}
interpolate_factor_along <- function(data, group, frame, frames, ease, history, keep_last) {
  all_levels <- levels(data)
  ord <- is.ordered(data)
  res <- interpolate_character_along(data, group, frame, frames, ease, history, keep_last)
  res[['data']] <- if (ord) ordered(res[['data']], all_levels) else factor(res[['data']], all_levels)
  res
}
interpolate_list_along <- function(data, group, frame, frames, ease, history, keep_last) {
  new_data <- list_along_interpolator(as.list(data), as.integer(group), as.numeric(frame), as.logical(history), as.logical(keep_last), as.numeric(frames), as.character(ease))
  attributes(new_data$data) <- attributes(data)
  new_data
}
interpolate_numlist_along <- function(data, group, frame, frames, ease, history, keep_last) {
  new_data <- numlist_along_interpolator(lapply(data, as.numeric), as.integer(group), as.numeric(frame), as.logical(history), as.logical(keep_last), as.numeric(frames), as.character(ease))
  attributes(new_data$data) <- attributes(data)
  new_data
}
get_phase_along <- function(group, frame, frames, history, keep_last) {
  phase_along_interpolator(as.integer(group), as.numeric(frame), as.logical(history), as.logical(keep_last), as.numeric(frames))
}
