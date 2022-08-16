#' Generator for tweening components separately from each other
#'
#' This is a generator versions of [tween_components()]. It returns a generator
#' that can be used with [get_frame()] and [get_raw_frames()] to extract frames
#' for a specific time point scaled between 0 and 1.
#'
#' @inheritParams tween_components
#'
#' @return A `component_generator` object
#'
#' @family Other generators
#'
#' @export
#' @importFrom rlang eval_tidy enquo quo_is_null
#'
#' @examples
#' from_zero <- function(x) {x$x <- 0; x}
#'
#' data <- data.frame(
#'   x = c(1, 2, 2, 1, 2, 2),
#'   y = c(1, 2, 2, 2, 1, 1),
#'   time = c(1, 4, 8, 4, 8, 10),
#'   id = c(1, 1, 1, 2, 2, 2)
#' )
#'
#' gen <- gen_components(data, 'cubic-in-out', time = time, id = id,
#'                       enter = from_zero, enter_length = 4)
#'
#' get_frame(gen, 0.3)
gen_components <- function(.data, ease, nframes, time, id = NULL, range = NULL,
                           enter = NULL, exit = NULL, enter_length = 0,
                           exit_length = 0) {
  time <- enquo(time)
  time <- eval_tidy(time, .data)
  id <- enquo(id)
  id <- if (quo_is_null(id)) rep(1, nrow(.data)) else eval_tidy(id, .data)
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

  gen_data <- .complete_components(.data, time, id, enter, exit, enter_length, exit_length)
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

#' @export
get_frame.component_generator <- function(generator, at, ...) {
  d <- generator_settings(generator)
  # clamp between 0 and 1
  at <- min(max(at, 0), 1)
  range <- d$range

  # normalise to range
  at <- (range[2] - range[1]) * at + range[1]

  data <- gen_data(generator)
  ease <- ease_type(generator)
  type <- col_types(generator)

  frame <- lapply(seq_along(data), function(i) {
    col <- data[[i]]
    e <- rep_len(ease[[i]], length(col))
    switch(
      type[i],
      numeric = interpolate_numeric_element_at(col, d$id, d$time, at, e),
      logical = interpolate_logical_element_at(col, d$id, d$time, at, e),
      factor = interpolate_factor_element_at(col, d$id, d$time, at, e),
      character = interpolate_character_element_at(col, d$id, d$time, at, e),
      colour = interpolate_colour_element_at(col, d$id, d$time, at, e),
      date = interpolate_date_element_at(col, d$id, d$time, at, e),
      datetime = interpolate_datetime_element_at(col, d$id, d$time, at, e),
      constant = interpolate_constant_element_at(col, d$id, d$time, at, e),
      numlist = interpolate_numlist_element_at(col, d$id, d$time, at, e),
      list = interpolate_list_element_at(col, d$id, d$time, at, e),
      phase = get_phase_element_at(col, d$id, d$time, at, e)
    )
  })
  structure(frame, names = names(data), row.names = .set_row_names(length(frame[[1]])), class = 'data.frame')
}
#' @export
get_raw_frames.component_generator <- function(generator, at, before = 0, after = 0, ...) {
  d <- generator_settings(generator)

  # clamp between 0 and 1
  before <- min(max(at - before, 0), 1)
  after <- min(max(at + after, 0), 1)
  at <- min(max(at, 0), 1)
  range <- d$range

  # normalise to generator time
  at <- (range[2] - range[1]) * at + range[1]
  before <- (range[2] - range[1]) * before + range[1]
  after <- (range[2] - range[1]) * after + range[1]

  # Find before and after
  before <- d$time >= before & d$time < at
  after <- d$time > at & d$time <= after
  raw <- d$data$.phase == 'raw'

  list(
    before = d$data[before & raw, , drop = FALSE],
    after = d$data[after & raw, , drop = FALSE]
  )
}


#' @export
convert_generator.component_generator <- convert_generator.along_generator
