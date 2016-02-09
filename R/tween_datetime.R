#' @include aaa.R
#'
NULL

#' @export
tween_datetime <- function(data, n, ease) {
    prepData <- prepareTween(data, n, ease)
    if (!all(sapply(prepData$data, inherits, what = 'POSIXt'))) {
        stop('data must consist of POSIXt elements')
    }
    tweendata <- do.call(interpolate_numeric_state, prepData)
    tweendata <- matrix(tweendata, ncol = length(prepData$data[[1]]), byrow = TRUE)
    rbind(tweendata, prepData$data[[length(prepData$data)]])
}
