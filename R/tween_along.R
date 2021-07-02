#' Interpolate data along a given dimension
#'
#' This tween takes groups of rows along with the time for each row and
#' calculates the exact value at each at each frame. Further it allows for
#' keeping the subsequent raw data from previous frame as well as letting the
#' final row linger beyond its time. It especially useful for data that should
#' be visualised as lines that are drawn along the x-axis, but can of course
#' also be used for other dimensions as well (even dimensions not corresponding
#' to any axis).
#'
#' @inheritParams tween_components
#' @param along The "time" point for each row
#' @param history Should earlier datapoints be kept in subsequent frames
#' @param keep_last Should the last point of each id be kept beyond its time
#'
#' @return A data.frame with the same columns as `.data` along with `.id` giving
#' the component id, `.phase` giving the state of each component in each frame,
#' and `.frame` giving the frame membership of each row.
#'
#' @family data.frame tween
#'
#' @importFrom rlang enquo quo_is_null eval_tidy
#' @export
tween_along <- function(.data, ease, nframes, along, id = NULL, range = NULL, history = TRUE, keep_last = FALSE) {
  along <- enquo(along)
  along <- as.numeric(eval_tidy(along, .data))
  id <- enquo(id)
  id <- if (quo_is_null(id)) rep(1, nrow(.data)) else eval_tidy(id, .data)
  .data <- .complete_along(.data, along, id)

  if (length(ease) == 1) ease <- rep(ease, ncol(.data) - 3)
  if (length(ease) == ncol(.data) - 3) {
    ease <- c(ease, 'linear', 'linear', 'linear') # To account for .phase and .id columns
  } else {
    stop('Ease must be either a single string or one for each column', call. = FALSE)
  }
  stopifnot(length(nframes) == 1 && is.numeric(nframes) && nframes %% 1 == 0)

  timerange <- if (is.null(range)) range(.data$.time) else range
  timerange <- as.numeric(timerange)
  if (diff(timerange) == 0) stop('range must have a length', call. = FALSE)
  framelength <- diff(timerange) / (nframes - 1)
  frame <- 1 + (nframes - 1) * (.data$.time - timerange[1]) / diff(timerange)
  frames <- seq_len(nframes)
  groups <- unique(.data$.id)
  group <- match(.data$.id, groups)
  colClasses <- col_classes(.data)
  tweendata <- lapply(seq_along(.data),  function(i) {
    d <- .data[[i]]
    e <- ease[i]
    switch(
      colClasses[i],
      numeric = interpolate_numeric_along(d, group, frame, frames, e, history, keep_last),
      logical = interpolate_logical_along(d, group, frame, frames, e, history, keep_last),
      factor = interpolate_factor_along(d, group, frame, frames, e, history, keep_last),
      character = interpolate_character_along(d, group, frame, frames, e, history, keep_last),
      colour = interpolate_colour_along(d, group, frame, frames, e, history, keep_last),
      date = interpolate_date_along(d, group, frame, frames, e, history, keep_last),
      datetime = interpolate_datetime_along(d, group, frame, frames, e, history, keep_last),
      constant = interpolate_constant_along(d, group, frame, frames, e, history, keep_last),
      numlist = interpolate_numlist_along(d, group, frame, frames, e, history, keep_last),
      list = interpolate_list_along(d, group, frame, frames, e, history, keep_last),
      phase = get_phase_along(group, frame, frames, history, keep_last)
    )
  })
  tweenInfo <- tweendata[[1]][, c('group', 'frame')]
  tweendata <- lapply(tweendata, `[[`, i = 'data')
  tweendata <- structure(tweendata, names = names(.data), row.names = seq_along(tweendata[[1]]), class = 'data.frame')
  tweendata$.frame <- tweenInfo$frame
  tweendata$.id <- tweenInfo$group
  attr(tweendata, 'framelength') <- framelength
  tweendata[order(tweendata$.frame, tweendata$.id), , drop = FALSE]
}

.complete_along <- function(data, along, id) {
  if (length(along) != nrow(data) || length(id) != nrow(data)) {
    stop('along and id must be the same length as the number of rows in data', call. = FALSE)
  }
  data <- data[order(id), , drop = FALSE]
  along <- along[order(id)]
  id <- sort(id)
  data$.id <- id
  data$.phase <- 'raw'
  data$.time <- along
  data
}
