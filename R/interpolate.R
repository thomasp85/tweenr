#' @include RcppExports.R
#'
NULL

## STATES

interpolate_numeric_state <- function(data, states) {
    numeric_state_interpolator(data, states)
}
#' @importFrom grDevices convertColor col2rgb rgb
interpolate_colour_state <- function(data, states) {
    data <- lapply(data, function(d){
        convertColor(t(col2rgb(d)), from = 'sRGB', to = 'Lab', scale.in = 255)
    })
    int_col <- colour_state_interpolator(data, states)
    int_col <- convertColor(int_col, from = 'Lab', to = 'sRGB', clip = TRUE)
    rgb(int_col[, 1], int_col[, 2], int_col[, 3])
}
interpolate_constant_state <- function(data, states) {
    constant_state_interpolator(data, states)
}
interpolate_character_state <- function(data, states) {
    interpolate_constant_state(data, states)
}
interpolate_date_state <- function(data, states) {
    data <- lapply(data, as.numeric)
    as.Date(interpolate_numeric_state(data, states), origin = BASEDATE)
}
interpolate_datetime_state <- function(data, states) {
    if (inherits(data[[1]], 'POSIXlt')) {
        warning("POSIXlt converted to POSIXct")
    }
    data <- lapply(data, as.numeric)
    as.POSIXct(interpolate_numeric_state(data, states), origin = BASEDATETIME)
}
interpolate_factor_state <- function(data, states) {
    data <- lapply(data, as.character)
    as.factor(interpolate_character_state(data, states))
}

## ELEMENTS

interpolate_numeric_element <- function(data, group, frame, ease) {
    numeric_element_interpolator(data, group, frame, ease)
}
#' @importFrom grDevices convertColor col2rgb rgb
interpolate_colour_element <- function(data, group, frame, ease) {
    data <- convertColor(t(col2rgb(data)), from = 'sRGB', to = 'Lab', scale.in = 255)
    int_col <- colour_element_interpolator(data, group, frame, ease)
    int_col_convert <- convertColor(as.matrix(int_col[, c('data1', 'data2', 'data3')]), from = 'Lab', to = 'sRGB', clip = TRUE)
    data.frame(
        data = rgb(int_col_convert[, 1], int_col_convert[, 2], int_col_convert[, 3]),
        group = int_col$group,
        frame = int_col$frame,
        stringsAsFactors = FALSE
    )
}
interpolate_constant_element <- function(data, group, frame, ease) {
    constant_element_interpolator(data, group, frame, ease)
}
interpolate_character_element <- function(data, group, frame, ease) {
    interpolate_constant_element(data, group, frame, ease)
}
interpolate_date_element <- function(data, group, frame, ease) {
    data <- as.numeric(data)
    res <- interpolate_numeric_element(data, group, frame, ease)
    res[['data']] <- as.Date(res[['data']], origin = BASEDATE)
    res
}
interpolate_datetime_element <- function(data, group, frame, ease) {
    if (inherits(data, 'POSIXlt')) {
        warning("POSIXlt converted to POSIXct")
    }
    data <- as.numeric(data)
    res <- interpolate_numeric_element(data, group, frame, ease)
    res[['data']] <-  as.POSIXct(res[['data']], origin = BASEDATETIME)
    res
}
interpolate_factor_element <- function(data, group, frame, ease) {
    data <- as.character(data)
    res <- interpolate_character_element(data, group, frame, ease)
    res[['data']] <- as.factor(res[['data']])
    res
}
