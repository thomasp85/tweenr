#' @include tween_numeric.R
#' @include tween_date.R
#' @include tween_datetime.R
#' @include tween_colour.R
#'
NULL

#' @export
tween <- function(data, n, ease = 'linear') {
    type <- guessType(data)
    switch(
        type,
        numeric = tween_numeric(data, n, ease),
        date = tween_date(data, n, ease),
        datetime = tween_datetime(data, n, ease),
        colour = tween_colour(data, n, ease),
        tween_constant_t(data, n, ease)
    )
}
#' @export
tween_t <- function(data, n, ease = 'linear') {
    type <- guessType(data)
    switch(
        type,
        numeric = tween_numeric_t(data, n, ease),
        date = tween_date_t(data, n, ease),
        datetime = tween_datetime_t(data, n, ease),
        colour = tween_colour_t(data, n, ease),
        tween_constant_t(data, n, ease)
    )
}

guessType <- function(data) {
    data <- unlist(data)
    if (is.character(data)) {
        if (anyNA(suppressWarnings(col2rgb(head(data, 100))))) {
            return('colour')
        }
    }
    if (inherits(data, 'Date')) {
        return('date')
    }
    if (inherits(data, 'POSIXt')) {
        return('datetime')
    }
    if (is.numeric(data)) {
        return('numeric')
    }
    'unknown'
}
