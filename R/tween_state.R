#' Compose tweening between states
#'
#' The `tween_state()` is a counterpart to `tween_states()` that is aimed at
#' letting you gradually build up a scene by composing state changes one by one.
#' This setup lets you take more control over each state change and allows you
#' to work with datasets with uneven number of rows, flexibly specifying what
#' should happen with entering and exiting data. `keep_state()` is a simpel
#' helper for letting you pause at a state. `open_state()` is a shortcut from
#' tweening from an empty dataset with a given `enter()` function while
#' `close_state()` is the same but will instead tween into an empty dataset with
#' a given `exit()` function.
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
#' matched by position. See the *Match, Enter, and Exit* section for more
#' information.
#'
#' @param enter,exit functions that calculate a start state for new observations
#' that appear in `to` or an end state for observations that are not present in
#' `to`. If `NULL` the new/old observations will not be part of the tween. The
#' function gets a data.frame with either the start state of the exiting
#' observations, or the end state of the entering observations and must return
#' a modified version of that data.frame. See the *Match, Enter, and Exit*
#' section for more information.
#'
#' @return A data.frame containing all the intermediary states in the tween,
#' each state will be enumerated by the `.frame` column
#'
#' @section Match, Enter, and Exit:
#' When there are discrepancies between the two states to tweeen between you
#' need a way to resolve the discrepancy before calculating the intermediary
#' states. With discrepancies we mean that some data points are present in the
#' start state and not in the end state, and/or some are present in the end
#' state but not in the start state. A simple example is that the start state
#' contains 100 rows and the end state contains 70. There are 30 missing rows
#' that we need to do something about before we can calculate the tween.
#'
#' **Making pairs**
#' The first question to answer is "How do we know which observations are
#' disappearing (*exiting*) and/or appearing (*entering*)?". This is done with
#' the `id` argument which should give a column name to match rows between the
#' two states on. If `id = NULL` the rows will be matched by position (in the
#' above example the last 30 rows in the start state will be entering). The `id`
#' column must only contain unique values in order to work.
#'
#' **Making up states**
#' Once the rows in each state has been paired you'll end up with three sets of
#' data. One containing rows that is present in both the start and end state,
#' one containing rows only present in the start state, and one only containing
#' rows present in the end state. The first group is easy - here you just tween
#' between each rows - but for the other two we'll need some state to start or
#' end the tween with. This is really the purpose of the `enter` and `exit`
#' functions. They take a data frame containing the subset of data that has not
#' been matched and must return a new data frame giving the state that these
#' rows must be tweened from/into. A simple example could be an `enter` function
#' that sets the variable giving the opacity in the plot to 0 - this will make
#' the new points fade into view during the transition.
#'
#' **Ignoring discrepancies**
#' The default values for `enter` and `exit` is `NULL`. This value indicate that
#' non-matching rows should simply be ignored for the transition and simply
#' appear in the last frame of the tween. This is the default.
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
#' # Using enter and exit (made up numbers)
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
#' pop_devel <- df1 %>%
#'   tween_state(df2, 'cubic-in-out', 50, id = 'country', enter = to_zero) %>%
#'   tween_state(df3, 'cubic-in-out', 50, id = 'country', enter = to_zero,
#'               exit = to_zero)
#'
tween_state <- function(.data, to, ease, nframes, id = NULL, enter = NULL, exit = NULL) {
    from <- .get_last_frame(.data)
    if (nrow(from) != nrow(.data)) nframes <- nframes + 1
    if (!setequal(names(from), names(to))) {
        stop('from and to must have identical columns', call. = FALSE)
    }
    to <- to[, match(names(from), names(to)), drop = FALSE]
    if (length(ease) == 1) ease <- rep(ease, ncol(from))
    if (length(ease) != ncol(from)) stop('Ease must be either a single string or one for each column', call. = FALSE)
    stopifnot(length(nframes) == 1 && is.numeric(nframes) && nframes %% 1 == 0)

    classes <- col_classes(from)
    stopifnot(identical(classes, col_classes(to)))
    full_set <- .complete_states(from, to, id, enter, exit)

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
            constant = interpolate_constant_state(d, state),
            numlist =,
            list = interpolate_list_state(d, state)
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
    .with_prior_frames(.data, tweendata)
}
#' @rdname tween_state
#' @export
keep_state <- function(.data, nframes) {
    state <- .get_last_frame(.data)
    if (nrow(state) != nrow(.data)) nframes <- nframes + 1
    states <- state[rep(seq_len(nrow(state)), nframes), , drop = FALSE]
    states$.frame <- rep(seq_len(nframes), each = nrow(state))
    .with_prior_frames(.data, states)
}
#' @rdname tween_state
#' @export
open_state <- function(.data, ease, nframes, enter) {
    to <- .get_first_frame(.data)
    if (nrow(to) != nrow(.data)) nframes <- nframes + 1
    from <- enter(to)
    tweendata <- tween_state(from, to, ease, nframes)
    .with_later_frames(.data, tweendata)
}
#' @rdname tween_state
#' @export
close_state <- function(.data, ease, nframes, exit) {
    from <- .get_last_frame(.data)
    if (nrow(from) != nrow(.data)) nframes <- nframes + 1
    to <- exit(from)
    tweendata <- tween_state(from, to, ease, nframes)
    .with_prior_frames(.data, tweendata)
}
#' Helpers for working with tweened data
#'
#' These are internal helpers for extracting and inserting data into a
#' data.frame of tweened states.
#'
#' @param data,prior,later A data.frame. If a `.frame` column exists it will be interpreted
#' as a data.frame containing multiple states
#'
#' @param new_tween The result of a tweening
#'
#' @return A data.frame
#' @keywords internal
#' @export
#'
.get_last_frame <- function(data) {
    if ('.frame' %in% names(data)) {
        data[data$.frame == max(data$.frame), names(data) != '.frame', drop = FALSE]
    } else {
        data
    }
}
#' @rdname dot-get_last_frame
#' @export
.get_first_frame <- function(data) {
    if ('.frame' %in% names(data)) {
        data[data$.frame == 1, names(data) != '.frame', drop = FALSE]
    } else {
        data
    }
}
#' @rdname dot-get_last_frame
#' @export
.with_prior_frames <- function(prior, new_tween) {
    if ('.frame' %in% names(prior)) {
        prior <- prior[prior$.frame != max(prior$.frame), , drop = FALSE]
        new_tween$.frame <- new_tween$.frame + max(prior$.frame)
        rbind(prior, new_tween)
    } else {
        new_tween
    }
}
#' @rdname dot-get_last_frame
#' @export
.with_later_frames <- function(later, new_tween) {
    if ('.frame' %in% names(later)) {
        later <- later[later$.frame != 1, , drop = FALSE]
        later$.frame <- later$.frame + max(new_tween$.frame)
        rbind(new_tween, later)
    } else {
        new_tween
    }
}
#' Fill in missing rows using enter and exit functions
#'
#' This function figures out which rows are missing in either state and applies
#' the provided `enter` and `exit`  functions to fill in the blanks and provide
#' a 1-to-1 relation between the rows in `from` and `to`.
#'
#' @param from,to Data.frames to tween between
#'
#' @param id The name of the column that holds the matching id
#'
#' @param enter,exit functions to fill out missing rows in `from` and `to`
#' respectively
#'
#' @return A list with the elements `from` and `to` holding the filled out
#' versions of `from` and `to`
#'
#' @keywords internal
#' @export
.complete_states <- function(from, to, id, enter, exit) {
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

        if (is.null(enter) || sum(entering) == 0) {
            to <- to[!entering, , drop = FALSE]
            to_id <- to_id[!entering]
            enters <- to[0, , drop = FALSE]
            enter_id <- to_id[0]
        } else {
            stopifnot(is.function(enter))
            enters <- enter(to[entering, , drop = FALSE])
            enter_id <- to_id[entering]
        }
        if (is.null(exit) || sum(exiting) == 0) {
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
