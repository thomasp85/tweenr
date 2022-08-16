#' Generator for keyframe based tweening
#'
#' This is a generator version of [tween_state()] and its utility functions. It
#' returns a generator that can be used with [get_frame()] and
#' [get_raw_frames()] to extract frames for a specific time point scaled between
#' 0 and 1.
#'
#' @param keyframe A data frame to use as a keyframe state
#' @inheritParams tween_state
#' @param pause The length of the pause at the current keyframe
#' @param length The length of the transition
#'
#' @return A `keyframe_generator` object
#'
#' @family Other generators
#'
#' @importFrom vctrs vec_cbind
#' @export
#'
#' @examples
#' df1 <- data.frame(
#'   country = c('Denmark', 'Sweden', 'Norway'),
#'   population = c(5e6, 10e6, 3.5e6)
#' )
#' df2 <- data.frame(
#'   country = c('Denmark', 'Sweden', 'Norway', 'Finland'),
#'   population = c(6e6, 10.5e6, 4e6, 3e6)
#' )
#' df3 <- data.frame(
#'   country = c('Denmark', 'Norway'),
#'   population = c(10e6, 6e6)
#' )
#' to_zero <- function(x) {
#'   x$population <- 0
#'   x
#' }
#' gen <- gen_keyframe(df1, 10) %>%
#'   add_keyframe(df2, 'cubic-in-out', 35, id = country, enter = to_zero) %>%
#'   add_pause(10) %>%
#'   add_keyframe(df3, 'cubic-in-out', 35, id = country, enter = to_zero,
#'                exit = to_zero) %>%
#'   add_pause(10)
#'
#' get_frame(gen, 0.25)
gen_keyframe <- function(keyframe = NULL, pause = 0) {
  if (is.null(keyframe)) {
    keyframe <- data.frame()
  }
  if (!is.data.frame(keyframe)) {
    stop("`start` must be `NULL` or a data frame", call. = FALSE)
  }
  keyframe$.phase <- rep_len(factor("raw", levels = PHASE_LEVELS), nrow(keyframe))
  class(keyframe) <- c(c("keyframe_generator", "frame_generator"), class(keyframe))
  generator_settings(keyframe) <- list(
    keyframes = list(list(keyframe, keyframe)),
    frame_time = c(0, pause),
    ease_type = list(),
    col_types = col_classes(keyframe)
  )
  keyframe
}

#' @rdname gen_keyframe
#' @export
add_pause <- function(.data, pause = 0) {
  cur_frame_time <- frame_times(.data)
  if (has_last_pause(.data)) {
    last(cur_frame_time) <- last(cur_frame_time) + pause
  } else {
    cur_frame_time <- c(cur_frame_time, pause + last(cur_frame_time))
  }
  frame_times(.data) <- cur_frame_time
  .data
}
#' @rdname gen_keyframe
#' @importFrom vctrs vec_cbind vec_rbind
#' @importFrom rlang enquo
#' @export
add_keyframe <- function(.data, keyframe, ease, length, id = NULL, enter = NULL, exit = NULL) {
  id <- enquo(id)
  .data <- prepare_keyframes(.data, keyframe)
  from <- last(keyframes(.data))[[2]]
  keyframe$.phase = rep_len(factor("raw", levels = PHASE_LEVELS), nrow(keyframe))
  full <- vec_rbind(from, keyframe)
  full <- .complete_states(
    full[seq_len(nrow(from)), ],
    full[-seq_len(nrow(from)), ],
    id,
    enter,
    exit,
    0
  )
  last(keyframes(.data))[[3]] <- full$from
  keyframes(.data) <- c(keyframes(.data), list(list(full$to, keyframe)))
  frame_times(.data) <- c(frame_times(.data), length + last(frame_times(.data)))
  ease_type(.data) <- c(ease_type(.data), list(ease))
  .data <- structure(
    vec_rbind(.data, keyframe),
    class = class(.data),
    generator_settings = generator_settings(.data)
  )
  col_types(.data) <- col_classes(.data)
  .data
}

#' @export
get_frame.keyframe_generator <- function(generator, at, ...) {
  # clamp between 0 and 1
  at <- min(max(at, 0), 1)
  ft <- frame_times(generator)

  # normalise to generator time
  at <- at * last(ft)

  # Find start stage
  stage <- max(first(which(at <= ft)) - 1, 1)
  if (at == 0) stage <- stage + 1
  key <- (stage + 1) %/% 2

  # if in pause, return the unaltered data (with "static" phase)
  if (stage %% 2 == 1 && !at %in% ft) {
    frame <- keyframes(generator)[[key]][[2]]
    frame$.phase <- factor("static", levels = PHASE_LEVELS)
    return(frame)
  }

  ease <- ease_type(generator)[[key]]
  pos <- (at - ft[stage]) / (ft[stage + 1] - ft[stage])
  frame_at(keyframes(generator)[[key]][[3]], keyframes(generator)[[key + 1]][[1]], pos, ease, col_types(generator))
}
#' @export
#' @importFrom vctrs vec_rbind
get_raw_frames.keyframe_generator <- function(generator, at, before = 0, after = 0, ...) {
  # clamp between 0 and 1
  before <- min(max(at - before, 0), 1)
  after <- min(max(at + after, 0), 1)
  at <- min(max(at, 0), 1)
  ft <- frame_times(generator)

  # normalise to generator time
  at <- at * last(ft)
  before <- before * last(ft)
  after <- after * last(ft)

  # Find before and after
  before <- which(ft >= before & ft < at)
  before <- unique((before + 1) %/% 2)
  after <- which(ft > at & ft <= after)
  after <- unique((after + 1) %/% 2)

  list(
    before = vec_rbind(lapply(keyframes(generator)[before], `[[`, 2)),
    after = vec_rbind(lapply(keyframes(generator)[after], `[[`, 2))
  )
}

#' @export
convert_generator.keyframe_generator <- function(x) {
  data <- keyframes(x)
  outer_id <- rep(seq_along(data), lengths(data))
  data <- unlist(data, recursive = FALSE)
  inner_id <- rep(seq_along(data), vapply(data, nrow, integer(1)))
  data <- vec_rbind(data)
  settings <- list(
    attributes = attributes(x),
    data = data.frame(x),
    inner_id = inner_id,
    outer_id = outer_id,
    converter = function(x, settings) {
      data <- settings$data
      attributes(data) <- settings$attributes
      keyframes(data) <- split(split(data.frame(x), settings$inner_id), settings$outer_id)
      data
    }
  )
  list(data = data, settings = settings)
}

# Utils -------------------------------------------------------------------

#' Prepare keyframes for generator
#'
#' @param .data A keyframe generator
#' @param keyframe A keyframe to add
#'
#' @return A valid keyframe generator
#'
#' @export
#' @keywords internal
#'
prepare_keyframes <- function(.data, keyframe) {
  if (!is.data.frame(keyframe)) {
    stop("`keyframe` must be a data frame", call. = FALSE)
  }
  if (!is_keyframe_generator(.data)) {
    stop("`.data` must be a keyframe generator", call. = FALSE)
  }
  if (is_keyframe_placeholder(.data)) {
    new_start <- generator_settings(keyframe[0, ])
    frame_times(new_start) <- frame_times(.data)
    .data <- new_start
  }
  if (!has_last_pause(.data)) {
    .data <- add_pause(.data, 0)
  }
  .data
}

is_keyframe_generator <- function(x) inherits(x, "keyframe_generator")
is_keyframe_placeholder <- function(x) isTRUE(names(x) == ".phase") && nrow(x) == 0

#' @rdname gen_internal
#' @export
keyframes <- function(x) generator_settings(x)$keyframes

#' @rdname gen_internal
#' @export
`keyframes<-` <- function(x, value) {
  generator_settings(x)$keyframes <- value
  x
}

#' @rdname gen_internal
#' @export
frame_times <- function(x) generator_settings(x)$frame_time

#' @rdname gen_internal
#' @export
`frame_times<-` <- function(x, value) {
  generator_settings(x)$frame_time <- value
  x
}

has_last_pause <- function(x) (length(frame_times(x)) %% 2) == 0

get_phase_at <- function(from, to) {
  factor(
    ifelse(from == "enter", "enter", ifelse(to == "exit", "exit", "transition")),
    levels = PHASE_LEVELS
  )
}

frame_at <- function(from, to, at, ease, classes) {
  if (at == 0) return(from)
  if (at == 1) return(to)
  at <- rep_len(at, nrow(from))
  ease <- rep_len(ease, ncol(from))
  data <- lapply(seq_along(classes), function(i) {
    switch(
      classes[i],
      numeric = interpolate_numeric_at(from[[i]], to[[i]], at, ease[i]),
      logical = interpolate_logical_at(from[[i]], to[[i]], at, ease[i]),
      factor = interpolate_factor_at(from[[i]], to[[i]], at, ease[i]),
      character = interpolate_character_at(from[[i]], to[[i]], at, ease[i]),
      colour = interpolate_colour_at(from[[i]], to[[i]], at, ease[i]),
      date = interpolate_date_at(from[[i]], to[[i]], at, ease[i]),
      datetime = interpolate_datetime_at(from[[i]], to[[i]], at, ease[i]),
      constant = interpolate_constant_at(from[[i]], to[[i]], at, ease[i]),
      numlist = interpolate_numlist_at(from[[i]], to[[i]], at, ease[i]),
      list = interpolate_list_at(from[[i]], to[[i]], at, ease[i]),
      phase = get_phase_at(from[[i]], to[[i]]),
      interpolate_custom_at(from[[i]], to[[i]], at, ease[i])
    )
  })
  structure(data, names = names(from), row.names = .set_row_names(length(data[[1]])), class = 'data.frame')
}

#' Fallback for keyframe vector support
#'
#' @param from,to vectors to interpolate between
#' @param at value between 0 and 1 defining the point
#' @param ease the easing function to use
#'
#' @export
#' @keywords internal
interpolate_custom_at <- function(from, to, at, ease) {
  UseMethod('interpolate_custom_at')
}
