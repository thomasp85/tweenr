#' @rdname tween
#'
#' @export
tween_numeric <- function(data, n, ease = 'linear') {
  data <- as.list(data)
  prepData <- prepareTween(data, n, ease)
  if (!all(sapply(prepData$data, is.numeric))) {
    stop('data must consist of numeric elements')
  }
  tweendata <- do.call(interpolate_numeric_state, prepData)
  unname(split(tweendata,
               rep(seq_along(data[[1]]), length.out = length(tweendata))))
}

#' @rdname tween
#'
#' @export
tween_numeric_t <- function(data, n, ease = 'linear') {
  if (!is.list(data)) {
    data <- list(data)
  }
  prepData <- prepareTweenTranspose(data, n, ease)
  if (!all(sapply(prepData$data, is.numeric))) {
    stop('data must consist of numeric elements')
  }
  tweendata <- do.call(interpolate_numeric_state, prepData)
  unname(split(tweendata,
               rep(seq_along(data), rep(n, length.out = length(data)))))
}
