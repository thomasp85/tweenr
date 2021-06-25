interpolate_numeric_element <- function(data, group, frame, ease) {
  numeric_element_interpolator(as.numeric(data), as.integer(group), as.integer(frame), as.character(ease))
}

interpolate_logical_element <- function(data, group, frame, ease) {
  res <- interpolate_numeric_element(data, group, frame, ease)
  res[['data']] <- as.logical(round(res[['data']]))
  res
}

#' @importFrom farver decode_colour encode_colour
interpolate_colour_element <- function(data, group, frame, ease) {
  data <- decode_colour(data, alpha = TRUE, to = 'lab')
  col <- colour_element_interpolator(data, as.integer(group), as.integer(frame), as.character(ease))
  data.frame(
    data = encode_colour(col[, 1:3, drop = FALSE], alpha = col[,4], from = 'lab'),
    group = col$group,
    frame = col$frame,
    stringsAsFactors = FALSE
  )
}

interpolate_constant_element <- function(data, group, frame, ease) {
  constant_element_interpolator(as.character(data), as.integer(group), as.integer(frame), as.character(ease))
}

interpolate_character_element <- interpolate_constant_element

interpolate_date_element <- function(data, group, frame, ease) {
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
  res <- interpolate_numeric_element(data, group, frame, ease)
  res[['data']] <-  as.POSIXct(res[['data']], origin = BASEDATETIME, tz = tz)
  res
}

interpolate_factor_element <- function(data, group, frame, ease) {
  all_levels <- levels(data)
  ord <- is.ordered(data)
  res <- interpolate_character_element(data, group, frame, ease)
  res[['data']] <- if (ord) ordered(res[['data']], all_levels) else factor(res[['data']], all_levels)
  res
}

interpolate_list_element <- function(data, group, frame, ease) {
  new_data <- list_element_interpolator(as.list(data), as.integer(group), as.integer(frame), as.character(ease))
  attributes(new_data$data) <- attributes(data)
  new_data
}

interpolate_numlist_element <- function(data, group, frame, ease) {
  new_data <- numlist_element_interpolator(lapply(data, as.numeric), as.integer(group), as.integer(frame), as.character(ease))
  attributes(new_data$data) <- attributes(data)
  new_data
}

get_phase_element <- function(data, group, frame, ease) {
  phase_element_interpolator(as.character(data), as.integer(group), as.integer(frame), as.character(ease))
}
