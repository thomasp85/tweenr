#' Transition in and out of events
#'
#' This tweening function is a more powerful version of [tween_appear()], with
#' support for newer features such as enter/exits and tween phase
#' identification. The tweener treats each row in the data as unique events in
#' time, and creates frames with the correct events present at any given time.
#'
#' @param start,end The start (and potential end) of the event encoded in the
#' row, as unquoted expressions. Will be evaluated in the context of `.data` so
#' can refer to columns in it. If `end = NULL` the event will be without extend
#' and only visible in a single frame, unless `enter` and/or `exit` is given.
#'
#' @inheritParams tween_components
#'
#' @return A data.frame with the same columns as `.data` along with `.id` giving
#' the component id, `.phase` giving the state of each component in each frame,
#' and `.frame` giving the frame membership of each row.
#'
#' @family data.frame tween
#'
#' @importFrom rlang enquo quo_is_missing eval_tidy
#' @export
#'
#' @examples
#' d <- data.frame(
#'   x = runif(20),
#'   y = runif(20),
#'   time = runif(20),
#'   duration = runif(20, max = 0.1)
#' )
#' from_left <- function(x) {
#'   x$x <- -0.5
#'   x
#' }
#' to_right <- function(x) {
#'   x$x <- 1.5
#'   x
#' }
#'
#' tween_events(d, 'cubic-in-out', 50, start = time, end = time + duration,
#'              enter = from_left, exit = to_right, enter_length = 0.1,
#'              exit_length = 0.05)
#'
tween_events <- function(.data, ease, nframes, start, end = NULL, range = NULL, enter = NULL, exit = NULL, enter_length = 0, exit_length = 0) {
  start <- enquo(start)
  if (quo_is_missing(start)) stop('start must be provided', call. = FALSE)
  start <- eval_tidy(start, .data)
  end <- enquo(end)
  end <- eval_tidy(end, .data)
  enter_length <- enquo(enter_length)
  enter_length <- eval_tidy(enter_length, .data)
  exit_length <- enquo(exit_length)
  exit_length <- eval_tidy(exit_length, .data)

  if (is.null(enter_length)) enter_length <- 0
  if (is.null(exit_length)) exit_length <- 0
  .data <- .complete_events(.data, start, end, enter, exit, enter_length, exit_length)

  .tween_individuals(.data, ease, nframes, range)
}

#' @importFrom vctrs vec_rbind
#' @importFrom rlang as_function
.complete_events <- function(data, start, end, enter, exit, enter_length, exit_length) {
  data$.id <- seq_len(nrow(data))
  data$.phase <- rep("raw", nrow(data))
  start <- rep(start, length.out = nrow(data))
  if (is.null(end)) {
    event_end <- data[0, , drop = FALSE]
    end <- start[0]
  } else {
    event_end <- data
    end <- rep(end, length.out = nrow(data))
    data$.phase <- 'static'
  }
  if (is.null(enter)) {
    enter_data <- data[0, , drop = FALSE]
    enter_time <- start[0]
  } else {
    enter_data <- as_function(enter)(data)
    enter_data$.phase <- 'enter'
    enter_time <- start - enter_length
  }
  if (is.null(exit)) {
    exit_data <- data[0, , drop = FALSE]
    exit_time <- start[0]
  } else {
    exit_data <- as_function(exit)(data)
    exit_data$.phase <- 'exit'
    exit_time <- (if (length(end) == 0) start else end) + exit_length
  }
  data <- vec_rbind(enter_data, data, event_end, exit_data)
  time <- c(enter_time, start, end, exit_time)
  data$.time <- time
  data
}
