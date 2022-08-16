#' Generator for tweening along a variable
#'
#' This is a generator version of [tween_along()]. It returns a generator that
#' can be used with [get_frame()] and [get_raw_frames()] to extract frames for
#' a specific time point scaled between 0 and 1.
#'
#' @inheritParams tween_along
#'
#' @return An `along_generator` object
#'
#' @family Other generators
#'
#' @export
#' @importFrom rlang eval_tidy enquo
#'
#' @examples
#' # Default behaviour
#' gen <- gen_along(airquality, ease = "linear", along = Day, id = Month)
#' get_frame(gen, 0.22)
#'
#' # Overwrite keep_last or history in get_frame
#' get_frame(gen, 0.67, history = FALSE)
gen_along <- function(.data, ease, along, id = NULL, range = NULL, history = TRUE, keep_last = FALSE) {
  along <- enquo(along)
  along <- as.numeric(eval_tidy(along, .data))
  id <- enquo(id)
  id <- if (quo_is_null(id)) rep(1, nrow(.data)) else eval_tidy(id, .data)

  .data$.phase <- NULL
  if (length(ease) == 1) ease <- rep(ease, ncol(.data))
  if (length(ease) == ncol(.data)) {
    ease <- c(ease, 'linear') # To account for .phase column
  } else {
    stop('Ease must be either a single string or one for each column', call. = FALSE)
  }

  .data$.phase <- rep_len(factor("raw", levels = PHASE_LEVELS), nrow(.data))
  d_order <- order(id, along)
  if (is.null(range)) range <- range(along)
  class(.data) <- c(c("along_generator", "frame_generator"), class(.data))
  generator_settings(.data) <- list(
    data = .data[d_order, ],
    id = id[d_order],
    along = along[d_order],
    range = range,
    ease_type = ease,
    history = history,
    keep_last = keep_last,
    col_types = col_classes(.data)
  )
  .data
}
#' @export
get_frame.along_generator <- function(generator, at, ..., history = NULL, keep_last = NULL) {
  d <- generator_settings(generator)
  # clamp between 0 and 1
  at <- min(max(at, 0), 1)
  range <- d$range

  # normalise to range
  at <- (range[2] - range[1]) * at + range[1]

  data <- gen_data(generator)
  ease <- ease_type(generator)
  type <- col_types(generator)
  if (is.null(history)) history <- d$history
  if (is.null(keep_last)) keep_last <- d$keep_last

  frame <- lapply(seq_along(data),  function(i) {
    data <- data[[i]]
    e <- ease[i]
    switch(
      type[i],
      numeric = interpolate_numeric_along(data, d$id, d$along, at, e, history, keep_last)$data,
      logical = interpolate_logical_along(data, d$id, d$along, at, e, history, keep_last)$data,
      factor = interpolate_factor_along(data, d$id, d$along, at, e, history, keep_last)$data,
      character = interpolate_character_along(data, d$id, d$along, at, e, history, keep_last)$data,
      colour = interpolate_colour_along(data, d$id, d$along, at, e, history, keep_last)$data,
      date = interpolate_date_along(data, d$id, d$along, at, e, history, keep_last)$data,
      datetime = interpolate_datetime_along(data, d$id, d$along, at, e, history, keep_last)$data,
      constant = interpolate_constant_along(data, d$id, d$along, at, e, history, keep_last)$data,
      numlist = interpolate_numlist_along(data, d$id, d$along, at, e, history, keep_last)$data,
      list = interpolate_list_along(data, d$id, d$along, at, e, history, keep_last)$data,
      phase = get_phase_along(d$id, d$along, at, history, keep_last)$data
    )
  })
  structure(frame, names = names(data), row.names = .set_row_names(length(frame[[1]])), class = 'data.frame')
}
#' @export
get_raw_frames.along_generator <- function(generator, at, before = 0, after = 0, ...) {
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
  before <- d$along >= before & d$along < at
  after <- d$along > at & d$along <= after

  list(
    before = d$data[before, , drop = FALSE],
    after = d$data[after, , drop = FALSE]
  )
}

#' @export
convert_generator.along_generator <- function(x) {
  data <- gen_data(x)
  settings <- list(
    attributes = attributes(x),
    data = data.frame(x),
    converter = function(x, settings) {
      data <- settings$data
      attributes(data) <- settings$attributes
      gen_data(data) <- data.frame(x)
      data
    }
  )
  list(data = data, settings = settings)
}
