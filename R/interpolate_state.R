interpolate_numeric_state <- function(data, states) {
  res <- numeric_state_interpolator(lapply(data, as.numeric), states)
  c(data[[1]][0], res)
}

interpolate_logical_state <- function(data, states) {
  res <- numeric_state_interpolator(lapply(data, as.numeric), states)
  as.logical(round(res))
}

#' @importFrom farver decode_colour encode_colour
interpolate_colour_state <- function(data, states) {
  data <- lapply(data, decode_colour, alpha = TRUE, to = 'lab')
  data <- colour_state_interpolator(data, states)
  encode_colour(data[, 1:3, drop = FALSE], alpha = data[,4], from = 'lab')
}

interpolate_constant_state <- function(data, states) {
  constant_state_interpolator(lapply(data, as.character), states)
}
interpolate_character_state <- interpolate_constant_state

interpolate_date_state <- function(data, states) {
  as.Date(interpolate_numeric_state(data, states), origin = BASEDATE)
}

interpolate_datetime_state <- function(data, states) {
  if (inherits(data[[1]], 'POSIXlt')) {
    warning("POSIXlt converted to POSIXct")
    data[[1]] <- as.POSIXct(data[[1]])
  }
  tz <- attr(data[[1]], 'tzone')
  as.POSIXct(interpolate_numeric_state(data, states), origin = BASEDATETIME, tz = tz)
}

interpolate_factor_state <- function(data, states) {
  all_levels <- Reduce(union, lapply(data, levels))
  ord <- is.ordered(data[[1]])
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
  phase_state_interpolator(lapply(data, as.character), states)
}
