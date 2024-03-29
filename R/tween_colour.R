#' @rdname tween
#'
#' @export
tween_colour <- function(data, n, ease = 'linear') {
  data <- as.list(data)
  prepData <- prepareTween(data, n, ease)
  tweendata <- do.call(interpolate_colour_state, prepData)
  unname(split(tweendata,
               rep(seq_along(data[[1]]), length.out = length(tweendata))))
}
#' @rdname tween
#'
#' @export
tween_color <- tween_colour

#' @rdname tween
#'
#' @export
tween_colour_t <- function(data, n, ease = 'linear') {
  if (!is.list(data)) {
    data <- list(data)
  }
  prepData <- prepareTweenTranspose(data, n, ease)
  tweendata <- do.call(interpolate_colour_state, prepData)
  unname(split(tweendata,
               rep(seq_along(data), rep(n, length.out = length(data)))))
}
#' @rdname tween
#'
#' @export
tween_color_t <- tween_colour_t
