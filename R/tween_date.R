#' @rdname tween
#'
#' @export
tween_date <- function(data, n, ease = 'linear') {
  data <- as.list(data)
  prepData <- prepareTween(data, n, ease)
  if (!all(sapply(prepData$data, inherits, what = 'Date'))) {
    stop('data must consist of Date elements')
  }
  tweendata <- do.call(interpolate_date_state, prepData)
  unname(split(tweendata,
               rep(seq_along(data[[1]]), length.out = length(tweendata))))
}

#' @rdname tween
#'
#' @export
tween_date_t <- function(data, n, ease = 'linear') {
  if (!is.list(data)) {
    data <- list(data)
  }
  prepData <- prepareTweenTranspose(data, n, ease)
  if (!all(sapply(prepData$data, inherits, what = 'Date'))) {
    stop('data must consist of Date elements')
  }
  tweendata <- do.call(interpolate_date_state, prepData)
  unname(split(tweendata,
               rep(seq_along(data), rep(n, length.out = length(data)))))
}
