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
#' might end up being higher depending on the regularity of `tweenlength`
#' and `statelength`.
#'
#' @return A data.frame with the same columns as the first data.frame in
#' `data`, but replicated `nframes` times. An additional column called
#' `.frame` will be added giving the frame number.
#'
#' @family data.frame tween
#'
#' @importFrom vctrs vec_cbind
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
  data <- lapply(data, function(d) {
    d$.phase <- 'raw'
    d
  })
  origNames <- names(data[[1]])
  if (!is.list(ease)) ease <- as.list(ease)
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
    state = NA_integer_,
    stringsAsFactors = FALSE
  )
  states$state <- rep(seq_len(nstates) - 1L, each = 2, length.out = nrow(states))
  states$ease <- lapply(c(rep(list('constant'), nstates), ease)[statesOrder], function(e) {
    structure(rep(e, length.out = length(origNames)), names = origNames)
  })
  fullLength <- sum(states$length)
  framelength <- fullLength/nframes
  states$nframes <- as.integer(round(states$length / framelength))
  nframes <- sum(states$nframes)
  framelength <- fullLength/nframes
  data <- Reduce(function(l, r) {
    extraCols <- !names(l[[length(l)]]) %in% names(r);
    append(l, list(vec_cbind(r, l[[length(l)]][, extraCols])))
  }, data[-1], data[1])
  colClasses <- col_classes(data[[1]])
  tweendata <- lapply(names(data[[1]]),  function(name) {
    d <- lapply(data, `[[`, i = name)
    d_states <- states
    d_states$ease <- vapply(d_states$ease, `[`, character(1), i = name)
    switch(
      colClasses[name],
      numeric = interpolate_numeric_state(d, d_states),
      logical = interpolate_logical_state(d, d_states),
      factor = interpolate_factor_state(d, d_states),
      character = interpolate_character_state(d, d_states),
      colour = interpolate_colour_state(d, d_states),
      date = interpolate_date_state(d, d_states),
      datetime = interpolate_datetime_state(d, d_states),
      constant = interpolate_constant_state(d, d_states),
      numlist = interpolate_numlist_state(d, d_states),
      list = interpolate_list_state(d, d_states),
      phase = get_phase_state(d, d_states)
    )
  })
  tweendata <- structure(tweendata, names = names(data[[1]]), row.names = .set_row_names(length(tweendata[[1]])), class = 'data.frame')
  tweendata$.id <- rep(seq_len(nrow(data[[1]])), each = nframes)
  tweendata$.frame <- rep(seq_len(nframes), each = nrow(data[[1]]))
  attr(tweendata, 'framelength') <- framelength
  tweendata
}
