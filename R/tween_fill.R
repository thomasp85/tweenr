#' Fill out missing values by interpolation
#'
#' This tween fills out `NA` elements (or `NULL` elements if `data` is a list)
#' by interpolating between the prior and next non-missing values.
#'
#' @param data A data.frame or vector.
#' @param ease A character vector giving valid easing functions. Recycled to
#' match the ncol of `data`
#'
#' @return If `data` is a data.frame then a data.frame with the same
#' columns. If `data` is a vector then a vector.
#'
#' @export
#'
#' @examples
#' # Single vector
#' tween_fill(c(1, NA, NA, NA, NA, NA, 2, 6, NA, NA, NA, -2), 'cubic-in-out')
#'
#' # Data frame
#' tween_fill(mtcars[c(1, NA, NA, NA, NA, 4, NA, NA, NA, 10), ], 'cubic-in')
#'
tween_fill <- function(data, ease) {
  single_vec <- !is.data.frame(data)
  if (single_vec) {
    if (length(data) == 0) return(data[integer()])
    data_df <- data.frame(data = rep(NA, length(data)))
    data_df$data <- data
    data <- data_df
  } else {
    if (nrow(data) == 0) return(data[integer(), ])
  }
  ease <- rep(ease, length.out = ncol(data))
  classes <- col_classes(data)
  tweendata <- lapply(seq_along(classes), function(i) {
    switch(
      classes[i],
      numeric = interpolate_numeric_fill(data[[i]], ease[i]),
      logical = interpolate_logical_fill(data[[i]], ease[i]),
      factor = interpolate_factor_fill(data[[i]], ease[i]),
      character = interpolate_character_fill(data[[i]], ease[i]),
      colour = interpolate_colour_fill(data[[i]], ease[i]),
      date = interpolate_date_fill(data[[i]], ease[i]),
      datetime = interpolate_datetime_fill(data[[i]], ease[i]),
      constant = interpolate_constant_fill(data[[i]], ease[i]),
      numlist = interpolate_numlist_fill(data[[i]], ease[i]),
      list = interpolate_list_fill(data[[i]], ease[i])
    )
  })
  if (single_vec) return(tweendata[[1]])

  structure(tweendata, names = names(data), row.names = seq_along(tweendata[[1]]), class = 'data.frame')
}
