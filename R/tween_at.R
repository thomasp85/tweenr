#' Get a specific position between two states
#'
#' This tween allows you to query a specific postion between two states rather
#' than generate evenly spaced states. It can work with either data.frames or
#' single vectors and each row/element can have its own position and easing.
#'
#' @param from,to A data.frame or vector of the same type. If either is of
#' length/nrow 1 it will get repeated to match the length of the other
#' @param at A numeric between 0 and 1 recycled to match the nrow/length of
#' `from`
#' @param ease A character vector giving valid easing functions. Recycled to
#' match the ncol of `from`
#'
#' @return If `from`/`to` is a data.frame then a data.frame with the same
#' columns. If `from`/`to` is a vector then a vector.
#'
#' @export
#'
#' @examples
#' tween_at(mtcars[1:6, ], mtcars[6:1, ], runif(6), 'cubic-in-out')
#'
tween_at <- function(from, to, at, ease) {
  single_vec <- !is.data.frame(from)
  if (single_vec) {
    if (length(from) == 0 || length(to) == 0) return(to[integer()])
    from_df <- data.frame(data = rep(NA, length(from)))
    to_df <- data.frame(data = rep(NA, length(to)))
    from_df$data <- from
    to_df$data <- to
    from <- from_df
    to <- to_df
  } else {
    if (nrow(from) == 0 || nrow(to) == 0) return(to[integer(), ])
  }
  if (length(at) == 0) stop('at must have length > 0', call. = FALSE)
  if (nrow(from) == 1) from <- from[rep(1, nrow(to)), , drop = FALSE]
  if (nrow(to) == 1) to <- to[rep(1, nrow(from)), , drop = FALSE]
  if (nrow(from) != nrow(to)) {
    stop('from and to must be same length', call. = FALSE)
  }
  if (any(names(from) != names(to))) {
    stop('`from` and `to` must have the same columns', call. = FALSE)
  }
  at <- rep(at, length.out = nrow(from))
  ease <- rep(ease, length.out = ncol(from))
  classes <- col_classes(from)
  to_classes <- col_classes(to)
  mismatch <- to_classes != classes
  for (i in which(mismatch)) {
    all_na_to <- all(is.na(to[[i]]))
    all_na_from <- all(is.na(from[[i]]))
    if (all_na_from) {
      storage.mode(from[[i]]) <- storage.mode(to[[i]])
    } else if (all_na_to) {
      storage.mode(to[[i]]) <- storage.mode(from[[i]])
    } else {
      stop('The ', names(to)[i], 'column differs in type between the two inputs', call. = FALSE)
    }
  }
  tweendata <- lapply(seq_along(classes), function(i) {
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
      phase = ifelse(from[[i]] == "enter", "enter", ifelse(to[[i]] == "exit", "exit", "transition"))
    )
  })
  if (single_vec) return(tweendata[[1]])

  structure(tweendata, names = names(from), row.names = .set_row_names(length(tweendata[[1]])), class = 'data.frame')
}
#' Get several specific position between two states
#'
#' This tween is a variation of [tween_at()]. Instead of having `at` refer to
#' the tweening position of each row, each `at` will interpolate the full data
#' at that position.
#'
#' @param from,to A data.frame or vector of the same type. If either is of
#' length/nrow 1 it will get repeated to match the length of the other
#' @param at A numeric vector with values between 0 and 1.
#' @param ease A character vector giving valid easing functions. Recycled to
#' match the ncol of `from`
#'
#' @return If `from`/`to` is a data.frame then a data.frame with the same
#' columns. If `from`/`to` is a vector then a vector.
#'
#' @export
#'
#' @examples
#' tween_at_t(mtcars[1:6, ], mtcars[6:1, ], runif(3), 'cubic-in-out')
#'
tween_at_t <- function(from, to, at, ease) {
  single_vec <- !is.data.frame(from)
  if (single_vec) {
    if (length(from) == 0 || length(to) == 0) return(to[integer()])
    from_df <- data.frame(data = rep(NA, length(from)))
    to_df <- data.frame(data = rep(NA, length(to)))
    from_df$data <- from
    to_df$data <- to
    from <- from_df
    to <- to_df
  } else {
    if (nrow(from) == 0 || nrow(to) == 0) return(to[integer(), ])
  }
  if (length(at) == 0) stop('at must have length > 0', call. = FALSE)
  if (nrow(from) == 1) from <- from[rep(1, nrow(to)), , drop = FALSE]
  if (nrow(to) == 1) to <- to[rep(1, nrow(from)), , drop = FALSE]
  if (nrow(from) != nrow(to)) {
    stop('from and to must be same length', call. = FALSE)
  }
  if (any(names(from) != names(to))) {
    stop('`from` and `to` must have the same columns', call. = FALSE)
  }
  ease <- rep(ease, length.out = ncol(from))
  classes <- col_classes(from)
  to_classes <- col_classes(to)
  mismatch <- to_classes != classes
  for (i in which(mismatch)) {
    all_na_to <- all(is.na(to[[i]]))
    all_na_from <- all(is.na(from[[i]]))
    if (all_na_from) {
      storage.mode(from[[i]]) <- storage.mode(to[[i]])
    } else if (all_na_to) {
      storage.mode(to[[i]]) <- storage.mode(from[[i]])
    } else {
      stop('The ', names(to)[i], 'column differs in type between the two inputs', call. = FALSE)
    }
  }
  tweendata <- lapply(seq_along(classes), function(i) {
    switch(
      classes[i],
      numeric = interpolate_numeric_at_t(from[[i]], to[[i]], at, ease[i]),
      logical = interpolate_logical_at_t(from[[i]], to[[i]], at, ease[i]),
      factor = interpolate_factor_at_t(from[[i]], to[[i]], at, ease[i]),
      character = interpolate_character_at_t(from[[i]], to[[i]], at, ease[i]),
      colour = interpolate_colour_at_t(from[[i]], to[[i]], at, ease[i]),
      date = interpolate_date_at_t(from[[i]], to[[i]], at, ease[i]),
      datetime = interpolate_datetime_at_t(from[[i]], to[[i]], at, ease[i]),
      constant = interpolate_constant_at_t(from[[i]], to[[i]], at, ease[i]),
      numlist = interpolate_numlist_at_t(from[[i]], to[[i]], at, ease[i]),
      list = interpolate_list_at_t(from[[i]], to[[i]], at, ease[i]),
      phase = interpolate_phase_at_t(from[[i]], to[[i]], at)
    )
  })
  if (single_vec) return(tweendata[[1]])

  tweendata$.frame <- rep(seq_along(at), each = length(tweendata[[1]]))

  structure(tweendata, names = names(from), row.names = .set_row_names(length(tweendata[[1]])), class = 'data.frame')
}

interpolate_phase_at_t <- function(from, to, at) {
  phase <- rep(ifelse(from == "enter", "enter", ifelse(to == "exit", "exit", "transition")), times = length(at))
  start_or_end <- at %in% c(0, 1)
  if (any(start_or_end)) {
    start_or_end <- rep(start_or_end, each = length(from))
    phase[phase == "transition" & start_or_end] <- "raw"
  }
  phase
}
