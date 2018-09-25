#' Create frames based on individual element states
#'
#' This function creates tweens for each observation individually, in cases
#' where the data doesn't pass through collective states but consists of fully
#' independent transitions. Each observation is identified by an id and each
#' state must have a time associated with it.
#'
#' @param data A data.frame consisting at least of a column giving the
#' observation id, a column giving timepoints for each state and a column giving
#' the easing to apply when transitioning away from the state.
#'
#' @param time The name of the column holding timepoints
#'
#' @param group The name of the column holding the observation id
#'
#' @param ease The name of the column holding the easing function name
#'
#' @param timerange The range of time to span. If missing it will default to
#' \code{range(data[[time]])}
#'
#' @param nframes The number of frames to generate. If missing it will default
#' to `ceiling(diff(timerange) + 1)` (At least one frame for each
#' individual timepoint)
#'
#' @return A data.frame with the same columns as `data` except for the
#' group and ease columns, but replicated `nframes` times. Two additional
#' columns called `.frame` and `.group` will be added giving the frame
#' number and observation id for each row.
#'
#' @family data.frame tween
#'
#' @examples
#' data <- data.frame(
#'   x = c(1, 2, 2, 1, 2, 2),
#'   y = c(1, 2, 2, 2, 1, 1),
#'   time = c(1, 4, 10, 4, 8, 10),
#'   group = c(1, 1, 1, 2, 2, 2),
#'   ease = rep('cubic-in-out', 6)
#' )
#'
#' data <- tween_elements(data, 'time', 'group', 'ease', nframes = 100)
#'
#' @export
#'
tween_elements <- function(data, time, group, ease, timerange, nframes) {
  if (!all(data[[ease]] %in% validEase)) {
    stop("All names given in the easing column must be valid easers")
  }

  if (missing(timerange) || is.null(timerange)) {
    timerange <- range(data[[time]])
  }
  if (missing(nframes) || is.null(nframes)) {
    nframes <- ceiling(diff(timerange) + 1)
  }
  framelength <- diff(timerange) / nframes
  specialCols <- c(group, ease)
  data <- data[order(data[[group]], data[[time]]), ]
  group <- as.character(data[[group]])
  frame <- round((data[[time]] - timerange[1]) / framelength)
  ease <- as.character(data[[ease]])
  data <- data[, !names(data) %in% specialCols, drop = FALSE]

  colClasses <- col_classes(data)
  tweendata <- lapply(seq_along(data),  function(i) {
    d <- data[[i]]
    switch(
      colClasses[i],
      numeric = interpolate_numeric_element(d, group, frame, ease),
      logical = interpolate_logical_element(d, group, frame, ease),
      factor = interpolate_factor_element(d, group, frame, ease),
      character = interpolate_character_element(d, group, frame, ease),
      colour = interpolate_colour_element(d, group, frame, ease),
      date = interpolate_date_element(d, group, frame, ease),
      datetime = interpolate_datetime_element(d, group, frame, ease),
      constant = interpolate_constant_element(d, group, frame, ease),
      numlist = interpolate_numlist_element(d, group, frame, ease),
      list = interpolate_list_element(d, group, frame, ease)
    )
  })
  tweenInfo <- tweendata[[1]][, c('group', 'frame')]
  tweendata <- as.data.frame(lapply(tweendata, `[[`, i = 'data'))
  names(tweendata) <- names(data)
  tweendata$.frame <- tweenInfo$frame
  tweendata$.group <- tweenInfo$group
  attr(tweendata, 'framelength') <- framelength
  tweendata[order(tweendata$.frame, tweendata$.group), ]
}
