#' @include RcppExports.R
#'
NULL

interpolate_numeric <- function(data, states) {
    numeric_interpolator(data, states)
}
#' @importFrom grDevices convertColor col2rgb rgb
interpolate_colour <- function(data, states) {
    data <- lapply(data, function(d){
        convertColor(t(col2rgb(d)), from = 'sRGB', to = 'Lab', scale.in = 255)
    })
    int_col <- colour_interpolator(data, states)
    int_col <- convertColor(int_col, from = 'Lab', to = 'sRGB', clip = TRUE)
    rgb(int_col[, 1], int_col[, 2], int_col[, 3])
}
interpolate_constant <- function(data, states) {
    constant_interpolator(data, states)
}
interpolate_character <- function(data, states) {
    interpolate_constant(data, states)
}
interpolate_date <- function(data, states) {
    data <- lapply(data, as.numeric)
    as.Date(interpolate_numeric(data, states), origin = BASEDATE)
}
interpolate_datetime <- function(data, states) {
    if (inherits(data[[1]], 'POSIXlt')) {
        warning("POSIXlt converted to POSIXct")
    }
    data <- lapply(data, as.numeric)
    as.POSIXct(interpolate_numeric(data, states), origin = BASEDATETIME)
}
interpolate_factor <- function(data, states) {
    data <- lapply(data, as.character)
    as.factor(interpolate_character(data, states))
}
