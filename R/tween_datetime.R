#' @rdname tween
#'
#' @export
tween_datetime <- function(data, n, ease = 'linear') {
  data <- as.list(data)
  prepData <- prepareTween(data, n, ease)
  if (!all(sapply(prepData$data, inherits, what = 'POSIXt'))) {
    stop('data must consist of POSIXt elements')
  }
  tweendata <- do.call(interpolate_datetime_state, prepData)
  unname(split(tweendata,
               rep(seq_along(data[[1]]), length.out = length(tweendata))))
}

#' @rdname tween
#'
#' @export
tween_datetime_t <- function(data, n, ease = 'linear') {
  if (!is.list(data)) {
    data <- list(data)
  }
  prepData <- prepareTweenTranspose(data, n, ease)
  if (!all(sapply(prepData$data, inherits, what = 'POSIXt'))) {
    stop('data must consist of POSIXt elements')
  }
  tweendata <- do.call(interpolate_datetime_state, prepData)
  unname(split(tweendata,
               rep(seq_along(data), rep(n, length.out = length(data)))))
}
