#' @include aaa.R
#'
NULL

#' @export
tween_elements <- function(data, time, group, ease, timerange, nframes) {
    if (!all(data[[ease]] %in% validEase)) {
        stop("All names given in the easing column must be valid easers")
    }

    if (missing(timerange) || is.null(timerange)) {
        timerange <- range(data[[time]])
    }
    if (missing(nframes) || is.null(nframes)) {
        nframes <- ceiling(diff(timerange) + 1)
    }
    framelength <- diff(timerange) / nframes
    specialCols <- c(group, ease)
    group <- as.character(data[[group]])
    data <- data[order(group, data[[time]]), ]
    frame <- round((data$time - timerange[1]) / framelength)
    ease <- as.character(data[[ease]])
    data <- data[, !names(data) %in% specialCols, drop = FALSE]

    colClasses <- col_classes(data)
    tweendata <- lapply(seq_along(data),  function(i) {
        d <- d[[i]]
        switch(
            colClasses[i],
            numeric = interpolate_numeric_element(d, group, frame, ease),
            factor = interpolate_factor_element(d, group, frame, ease),
            character = interpolate_character_element(d, group, frame, ease),
            colour = interpolate_colour_element(d, group, frame, ease),
            date = interpolate_date_element(d, group, frame, ease),
            datetime = interpolate_datetime_element(d, group, frame, ease),
            constant = interpolate_constant_element(d, group, frame, ease)
        )
    })
    tweenInfo <- tweendata[[1]][, c('group', 'frame')]
    tweendata <- as.data.frame(lapply(tweendata, `[[`, i = 'data'))
    names(tweendata) <- names(data)
    tweendata$.frame <- tweenInfo$frame
    tweendata$.group <- tweenInfo$group
    attr(tweendata, 'framelength') <- framelength
    tweendata
}
