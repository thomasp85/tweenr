#' Generator for tweening the appearance of elements
#'
#' This is a generator version of [tween_events()]. It returns a generator
#' that can be used with [get_frame()] and [get_raw_frames()] to extract frames
#' for a specific time point scaled between 0 and 1.
#'
#' @inheritParams tween_events
#'
#' @return A `component_generator` object
#'
#' @family Other generators
#'
#' @export
#' @importFrom rlang enquo eval_tidy quo_is_missing
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
#' gen <- gen_events(d, 'cubic-in-out', start = time, end = time + duration,
#'                   enter = from_left, exit = to_right, enter_length = 0.1,
#'                   exit_length = 0.05)
#'
#' get_frame(gen, 0.65)
#'
gen_events <- function(.data, ease, start, end = NULL, range = NULL, enter = NULL, exit = NULL, enter_length = 0, exit_length = 0) {
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

  .data$.phase <- NULL
  if (length(ease) == 1) ease <- rep(ease, ncol(.data))
  if (length(ease) == ncol(.data)) {
    ease <- c(ease, 'linear') # To account for .phase column
  } else {
    stop('Ease must be either a single string or one for each column', call. = FALSE)
  }

  .data$.phase <- rep_len(factor("raw", levels = PHASE_LEVELS), nrow(.data))
  class(.data) <- c(c("component_generator", "frame_generator"), class(.data))

  gen_data <- .complete_events(.data, start, end, enter, exit, enter_length, exit_length)

  time <- gen_data$.time
  id <- gen_data$.id
  gen_data$.time <- NULL
  gen_data$.id <- NULL
  d_order <- order(id, time)
  if (is.null(range)) range <- range(time)
  if (diff(range) == 0) stop('range cannot be 0', call. = FALSE)

  generator_settings(.data) <- list(
    data = gen_data[d_order, ],
    id = id[d_order],
    time = time[d_order],
    range = range,
    ease_type = ease,
    col_types = col_classes(.data)
  )

  .data
}
