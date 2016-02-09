#' @include aaa.R
#'
NULL

#' @export
tween_colour <- function(data, n, ease) {
    prepData <- prepareTween(data, n, ease)
    if (anyNA(suppressWarnings(col2rgb(unlist(prepData$data))))) {
        stop('all elements in data must be convertible to colour')
    }
    tweendata <- do.call(interpolate_colour_state, prepData)
    tweendata <- matrix(tweendata, ncol = length(prepData$data[[1]]), byrow = TRUE)
    endpoint <- col2rgb(prepData$data[[length(prepData$data)]]) / 255
    rbind(tweendata, rgb(endpoint[1,], endpoint[2,], endpoint[3,]))
}
#' @export
tween_color <- tween_colour
