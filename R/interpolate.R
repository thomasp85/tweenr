## STATES

interpolate_numeric_state <- function(data, states) {
    numeric_state_interpolator(data, states)
}
interpolate_logical_state <- function(data, states) {
    as.logical(round(numeric_state_interpolator(lapply(data, as.numeric), states)))
}
#' @importFrom grDevices col2rgb rgb
#' @importFrom farver convert_colour
interpolate_colour_state <- function(data, states) {
    data <- lapply(data, function(d){
        col <- t(col2rgb(d, alpha = TRUE))
        col_conv <- convert_colour(col[,1:3, drop = FALSE], from = 'rgb', to = 'lab')
        cbind(col_conv, col[,4])
    })
    int_col <- colour_state_interpolator(data, states)
    alpha <- int_col[,4]
    alpha[alpha > 255] <- 255
    alpha[alpha < 0] <- 0
    int_col <- convert_colour(int_col[, 1:3, drop = FALSE], from = 'lab', to = 'rgb')
    int_col[int_col > 255] <- 255
    int_col[int_col < 0] <- 0
    rgb(int_col[, 1], int_col[, 2], int_col[, 3], alpha, maxColorValue = 255)
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
    all_levels <- Reduce(union, lapply(data, levels))
    data <- lapply(data, as.character)
    factor(interpolate_character_state(data, states), all_levels)
}
interpolate_list_state <- function(data, states) {
    new_data <- list_state_interpolator(data, states)
    attributes(new_data) <- attributes(data)
    new_data
}
interpolate_numlist_state <- function(data, states) {
    new_data <- numlist_state_interpolator(lapply(data, lapply, as.numeric), states)
    attributes(new_data) <- attributes(data)
    new_data
}

## ELEMENTS

interpolate_numeric_element <- function(data, group, frame, ease) {
    numeric_element_interpolator(data, group, frame, ease)
}
interpolate_logical_element <- function(data, group, frame, ease) {
    res <- numeric_element_interpolator(as.numeric(data), group, frame, ease)
    res[['data']] <- as.logical(round(res[['data']]))
    res
}
#' @importFrom grDevices col2rgb rgb
#' @importFrom farver convert_colour
interpolate_colour_element <- function(data, group, frame, ease) {
    col <- t(col2rgb(data, alpha = TRUE))
    data <- convert_colour(col[,1:3, drop = FALSE], from = 'rgb', to = 'lab')
    int_col <- colour_element_interpolator(cbind(data, col[,4]), group, frame, ease)
    int_col_convert <- convert_colour(as.matrix(int_col[, c('data1', 'data2', 'data3')]), from = 'lab', to = 'rgb')
    int_col_convert[int_col_convert > 255] <- 255
    int_col_convert[int_col_convert < 0] <- 0
    int_col$data4[int_col$data4 < 0] <- 0
    data.frame(
        data = rgb(int_col_convert[, 1], int_col_convert[, 2], int_col_convert[, 3], int_col$data4, maxColorValue = 255),
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
    all_levels <- levels(data)
    data <- as.character(data)
    res <- interpolate_character_element(data, group, frame, ease)
    res[['data']] <- factor(res[['data']], all_levels)
    res
}
interpolate_list_element <- function(data, group, frame, ease) {
    new_data <- list_element_interpolator(data, group, frame, ease)
    attributes(new_data$data) <- attributes(data)
    new_data
}
interpolate_numlist_element <- function(data, group, frame, ease) {
    new_data <- numlist_element_interpolator(data, group, frame, ease)
    attributes(new_data$data) <- attributes(data)
    new_data
}
