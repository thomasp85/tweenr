#' Interpolate individual component
#'
#' This function is much like [tween_elements()] but with a slightly different
#' syntax and support for many of the newer features such as enter/exits and
#' tween phase identification. Furthermore it uses tidy evaluation for time and
#' id, making it easier to change these on the fly. The biggest change in terms
#' of functionality compared to `tween_elements()` is that the easing function
#' is now given per column and not per row. If different easing functions are
#' needed for each transition then `tween_elements()` is needed.
#'
#' @inheritParams tween_state
#'
#' @param .data A data.frame with components at different stages
#'
#' @param time An unquoted expression giving the timepoint for the different
#' stages of the components. Will be evaluated in the context of `.data` so can
#' refer to a column from that
#'
#' @param id An unquoted expression giving the component id for each row. Will
#' be evaluated in the context of `.data` so can refer to a column from that
#'
#' @param enter_length,exit_length The lenght of the opening and closing
#' transitions if `enter` and/or `exit` is given. Measured in the same units as
#' `time`
#'
#' @return A data.frame with the same columns as `data` along with `.id` giving
#' the component id, `.phase` giving the state of each component in each frame,
#' and `.frame` giving the frame membership of each row.
#'
#' @family data.frame tween
#'
#' @examples
#'
#' from_zero <- function(x) {x$x <- 0; x}
#'
#' data <- data.frame(
#'   x = c(1, 2, 2, 1, 2, 2),
#'   y = c(1, 2, 2, 2, 1, 1),
#'   time = c(1, 4, 10, 4, 8, 10),
#'   id = c(1, 1, 1, 2, 2, 2)
#' )
#'
#' data <- tween_components(data, 'cubic-in-out', nframes = 100, time = time,
#'                          id = id, enter = from_zero, enter_length = 4)
#'
#' @export
#' @importFrom rlang enquo eval_tidy
#'
tween_components <- function(.data, ease, nframes, time, id, enter = NULL, exit = NULL, enter_length = 0, exit_length = 0) {
    time <- enquo(time)
    time <- eval_tidy(time, data)
    id <- enquo(id)
    id <- eval_tidy(id, data)
    .data <- .complete_components(.data, time, id, enter, exit, enter_length, exit_length)

    .tween_individuals(.data, ease, nframes)
}

.tween_individuals <- function(.data, ease, nframes) {
    if (length(ease) == 1) ease <- rep(ease, ncol(.data) - 2)
    if (length(ease) == ncol(.data) - 2) {
        ease <- c(ease, 'linear', 'linear') # To account for .phase and .id columns
    } else {
        stop('Ease must be either a single string or one for each column', call. = FALSE)
    }
    stopifnot(length(nframes) == 1 && is.numeric(nframes) && nframes %% 1 == 0)

    timerange <- range(.data$.time)
    framelength <- diff(timerange) / (nframes - 1)
    .data <- .data[order(.data$.id, .data$.time), , drop = FALSE]
    frame <- round((.data$.time - min(timerange[1])) / framelength) + 1
    frame[frame < 1] <- 1
    frame[frame > nframes] <- nframes
    .data$.time <- NULL
    colClasses <- col_classes(.data)
    tweendata <- lapply(seq_along(.data),  function(i) {
        d <- .data[[i]]
        e <- rep(ease[i], length(d))
        switch(
            colClasses[i],
            numeric = interpolate_numeric_element(d, .data$.id, frame, e),
            logical = interpolate_logical_element(d, .data$.id, frame, e),
            factor = interpolate_factor_element(d, .data$.id, frame, e),
            character = interpolate_character_element(d, .data$.id, frame, e),
            colour = interpolate_colour_element(d, .data$.id, frame, e),
            date = interpolate_date_element(d, .data$.id, frame, e),
            datetime = interpolate_datetime_element(d, .data$.id, frame, e),
            constant = interpolate_constant_element(d, .data$.id, frame, e),
            numlist = interpolate_numlist_element(d, .data$.id, frame, e),
            list = interpolate_list_element(d, .data$.id, frame, e),
            phase = get_phase_element(d, .data$.id, frame, e)
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

.complete_components <- function(data, time, id, enter, exit, enter_length, exit_length) {
    data$.id <- id
    data$.phase <- 'raw'
    data$.time <- time
    if (any(!is.null(enter), !is.null(exit))) {
        time_ord <- order(time)
        if (!is.null(enter)) {
            enter_data <- enter(data[time_ord[!duplicated(id[time_ord])], , drop = FALSE])
            enter_data$.phase <- 'enter'
            enter_data$.time <- enter_data$.time - enter_length
        } else {
            enter_data <- data[0, , drop = FALSE]
        }
        if (!is.null(exit)) {
            exit_data <- enter(data[time_ord[!duplicated(id[time_ord], fromLast = TRUE)], , drop = FALSE])
            exit_data$.phase <- 'exit'
            exit_data$.time <- exit_data$.time + exit_length
        } else {
            exit_data <- data[0, , drop = FALSE]
        }
        data <- rbind(enter_data, data, exit_data)
    }
    data
}
