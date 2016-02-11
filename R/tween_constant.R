#' @include aaa.R
#'
NULL

#' @rdname tween
#'
#' @export
tween_constant <- function(data, n, ease = 'linear') {
    data <- as.list(data)
    data <- lapply(data, as.character)
    prepData <- prepareTween(data, n, ease)
    tweendata <- do.call(interpolate_character_state, prepData)
    unname(split(tweendata,
                 rep(seq_along(data[[1]]), length.out = length(tweendata))))
}

#' @rdname tween
#'
#' @export
tween_constant_t <- function(data, n, ease = 'linear') {
    if (!is.list(data)) {
        data <- list(data)
    }
    data <- lapply(data, as.character)
    prepData <- prepareTweenTranspose(data, n, ease)
    tweendata <- do.call(interpolate_character_state, prepData)
    unname(split(tweendata,
                 rep(seq_along(data), rep(n, length.out = length(data)))))
}
