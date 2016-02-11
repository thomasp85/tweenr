#' @include aaa.R
#' @include interpolate.R
#'
NULL

#' Tween a list of data.frames representing states
#'
#' This function is intended to create smooth transitions between states of
#' data. States are defined as full data.frames or data.frames containing only
#' the columns with change. Each state can have a defined period of pause, the
#' transition length between each states can be defined as well as the easing
#' function.
#'
#' @param data A list of data.frames. Each data.frame must contain the same
#' number of rows, but only the first data.frame needs to contain all columns.
#' Subsequent data.frames need only contain the columns that shows change.
#'
#' @param tweenlength The lengths of the transitions between each state.
#'
#' @param statelength The length of the pause at each state.
#'
#' @param ease The easing functions to use for the transitions. See details.
#'
#' @param nframes The number of frames to generate. The actual number of frames
#' might end up being higher depending on the regularity of \code{tweenlength}
#' and \code{statelength}.
#'
#' @return A data.frame with the same columns as the first data.frame in
#' \code{data}, but replicated \code{nframes} times. An additional column called
#' \code{.frame} will be added giving the frame number.
#'
#' @family data.frame tween
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
#' data <- tween_states(list(data1, data2), 3, 1, 'cubic-in-out', 100)
#'
#' @export
#'
tween_states <- function(data, tweenlength, statelength, ease, nframes) {
    if (!(is.list(data) && all(sapply(data, is.data.frame)))) {
        stop('data must be a list of data.frames')
    }
    if (length(data) == 1) {
        stop('data must contain multiple states')
    }
    if (length(unique(sapply(data, nrow))) != 1) {
        stop('All elements in data must have the same number of rows')
    }
    origNames <- names(data[[1]])
    allNames <- unlist(lapply(data, names))
    if (!all(allNames %in% origNames)) {
        stop('All columns must be specified in the original data.frame')
    }
    nstates <- length(data)
    tweenlength <- rep(tweenlength, nstates)[seq_len(nstates - 1)]
    statelength <- rep(statelength, nstates)[seq_len(nstates)]
    ease <- rep(ease, nstates)[seq_len(nstates - 1)]
    pauseIndex <- which(rep(c(TRUE, FALSE), length.out = 2*nstates - 1))
    tweenIndex <- which(rep(c(FALSE, TRUE), length.out = 2*nstates - 1))
    statesOrder <- order(c(pauseIndex, tweenIndex))
    states <- data.frame(
        length = c(statelength, tweenlength)[statesOrder],
        nframes = NA_integer_,
        ease = c(rep('constant', nstates), ease)[statesOrder],
        state = NA_integer_,
        stringsAsFactors = FALSE
    )
    states$state <- rep(seq_len(nstates) - 1, each = 2, length.out = nrow(states))
    fullLength <- sum(states$length)
    framelength <- fullLength/nframes
    states$nframes <- ceiling(states$length / framelength)
    nframes <- sum(states$nframes)
    framelength <- fullLength/nframes
    data <- Reduce(function(l, r) {
        extraCols <- !names(l[[length(l)]]) %in% names(r);
        append(l, list(cbind(r, l[[length(l)]][, extraCols])))
    }, data[-1], data[1])
    colClasses <- col_classes(data[[1]])
    tweendata <- lapply(names(data[[1]]),  function(name) {
        d <- lapply(data, `[[`, i = name)
        switch(
            colClasses[name],
            numeric = interpolate_numeric_state(d, states),
            factor = interpolate_factor_state(d, states),
            character = interpolate_character_state(d, states),
            colour = interpolate_colour_state(d, states),
            date = interpolate_date_state(d, states),
            datetime = interpolate_datetime_state(d, states),
            constant = interpolate_constant_state(d, states)
        )
    })
    tweendata <- as.data.frame(tweendata)
    names(tweendata) <- names(data[[1]])
    tweendata$.frame <- rep(seq_len(nframes), each = nrow(data[[1]]))
    attr(tweendata, 'framelength') <- framelength
    tweendata
}
