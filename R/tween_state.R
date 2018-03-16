#' Compose tweening between states
#'
#' The `tween_state()` is a counterpart to `tween_states()` that is aimed at
#' letting you gradually build up a scene by composing state changes one by one.
#' This setup lets you take more control over each state change and allows you
#' to work with datasets with uneven number of rows, flexibly specifying what
#' should happen with entering and exiting data. `keep_state()` is a simpel
#' helper for letting you pause at a state.
#'
#' @param .data A data.frame to start from. If `.data` is the result of a prior
#' tween, only the last frame will be used for the tween. The new tween will
#' then be added to the prior tween
#'
#' @param to A data.frame to end at. It must contain the same columns as .data
#' (exluding `.frame`)
#'
#' @param ease The easing function to use. Either a single string or one for
#' each column in the data set.
#'
#' @param nframes The number of frames to calculate for the tween
#'
#' @param id The column to match observations on. If `NULL` observations will be
#' matched by position
#'
#' @param enter,exit functions that calculate a start state for new observations
#' that appear in `to` or an end state for observations that are not present in
#' `to`. If `NULL` the new/old observations will not be part of the tween. The
#' function gets a data.frame with either the start state of the exiting
#' observations, or the end state of the entering observations and must return
#' a modified version of that data.frame.
#'
#' @return A data.frame containing all the intermediary states in the tween,
#' each state will be enumerated by the `.frame` column
#'
#' @export
#'
#' @examples
#' data1 <- data.frame(
#'   x = 1:20,
#'   y = 0,
#'   colour = 'forestgreen',
#'   stringsAsFactors = FALSE
#' )
#' data2 <- data1
#' data2$x <- 20:1
#' data2$y <- 1
#'
#' data <- data1 %>%
#'   tween_state(data2, 'linear', 50) %>%
#'   keep_state(20) %>%
#'   tween_state(data1, 'bounce-out', 50)
#'
tween_state <- function(.data, to, ease, nframes, id = NULL, enter = NULL, exit = NULL) {
    from <- get_last_frame(.data)
    if (nrow(from) != nrow(.data)) nframes <- nframes + 1
    if (!setequal(names(from), names(to))) {
        stop('from and to must have identical columns', call. = FALSE)
    }
    to <- to[, match(names(from), names(to))]
    if (length(ease) == 1) ease <- rep(ease, ncol(from))
    if (length(ease) != ncol(from)) stop('Ease must be either a single string or one for each column', call. = FALSE)
    stopifnot(length(nframes) == 1 && is.numeric(nframes) && nframes %% 1 == 0)

    classes <- col_classes(from)
    stopifnot(identical(classes, col_classes(to)))
    full_set <- complete_states(from, to, id, enter, exit)

    tweendata <- lapply(seq_along(classes), function(i) {
        d <- list(full_set$from[[i]], full_set$to[[i]])
        state <- simple_state(nframes, ease[i])
        switch(
            classes[i],
            numeric = interpolate_numeric_state(d, state),
            factor = interpolate_factor_state(d, state),
            character = interpolate_character_state(d, state),
            colour = interpolate_colour_state(d, state),
            date = interpolate_date_state(d, state),
            datetime = interpolate_datetime_state(d, state),
            constant = interpolate_constant_state(d, state)
        )
    })
    tweendata <- as.data.frame(tweendata)
    names(tweendata) <- names(from)
    tweendata$.frame <- rep(seq_len(nframes - 1), each = nrow(full_set$from))
    tweendata <- rbind(
        cbind(from, .frame = 1),
        tweendata[tweendata$.frame != 1, , drop = FALSE],
        cbind(to, .frame = nframes)
    )
    with_prior_frames(.data, tweendata)
}
#' @rdname tween_state
#' @export
keep_state <- function(.data, nframes) {
    state <- get_last_frame(.data)
    if (nrow(state) != nrow(.data)) nframes <- nframes + 1
    states <- state[rep(seq_len(nrow(state)), nframes), , drop = FALSE]
    states$.frame <- rep(seq_len(nframes), each = nrow(state))
    with_prior_frames(.data, states)
}

get_last_frame <- function(data) {
    if ('.frame' %in% names(data)) {
        data[data$.frame == max(data$.frame), names(data) != '.frame']
    } else {
        data
    }
}
with_prior_frames <- function(prior, new_tween) {
    if ('.frame' %in% names(prior)) {
        prior <- prior[prior$.frame != max(prior$.frame), , drop = FALSE]
        new_tween$.frame <- new_tween$.frame + max(prior$.frame)
        rbind(prior, new_tween)
    } else {
        new_tween
    }
}
complete_states <- function(from, to, id, enter, exit) {
    if (is.null(id)) {
        from_id <- seq_len(nrow(from))
        to_id <- seq_len(nrow(to))
    } else {
        stopifnot(id %in% names(from))
        from_id <- from[[id]]
        to_id <- to[[id]]
    }
    if (!setequal(from_id, to_id)) {
        entering <- !to_id %in% from_id
        exiting <- !from_id %in% to_id
        exits <- from[entering, , drop = FALSE]

        if (is.null(enter)) {
            to <- to[!entering, , drop = FALSE]
            to_id <- to_id[!entering]
            enters <- to[0, , drop = FALSE]
            enter_id <- to_id[0]
        } else {
            stopifnot(is.function(enter))
            enters <- enter(to[entering, , drop = FALSE])
            enter_id <- to_id[entering]
        }
        if (is.null(exit)) {
            from <- from[!exiting, , drop = FALSE]
            from_id <- from_id[!entering]
            exits <- from[0, , drop = FALSE]
            exit_id <- from_id[0]
        } else {
            stopifnot(is.function(exit))
            exits <- exit(from[exiting, , drop = FALSE])
            exit_id <- from_id[exiting]
        }
        from <- rbind(from, enters)
        from_id <- c(from_id, enter_id)
        to <- rbind(to, exits)
        to_id <- c(to_id, exit_id)
    }
    to <- to[match(from_id, to_id), , drop = FALSE]

    list(from = from, to = to)
}
simple_state <- function(n, ease) {
    data.frame(state = c(0, 1), nframes = c(n - 1, 0), ease = c(ease, 'constant'), stringsAsFactors = FALSE)
}
