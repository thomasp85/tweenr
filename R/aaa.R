BASEDATE <- Sys.Date() - as.numeric(Sys.Date())
BASEDATETIME <- Sys.time() - as.numeric(Sys.time())

validEase <- c(
    "linear",
    "quadratic-in",
    "quadratic-out",
    "quadratic-in-out",
    "cubic-in",
    "cubic-out",
    "cubic-in-out",
    "quartic-in",
    "quartic-out",
    "quartic-in-out",
    "quintic-in",
    "quintic-out",
    "quintic-in-out",
    "sine-in",
    "sine-out",
    "sine-in-out",
    "circular-in",
    "circular-out",
    "circular-in-out",
    "exponential-in",
    "exponential-out",
    "exponential-in-out",
    "elastic-in",
    "elastic-out",
    "elastic-in-out",
    "back-in",
    "back-out",
    "back-in-out",
    "bounce-in",
    "bounce-out",
    "bounce-in-out"
)

col_classes <- function(data) {
    classes <- sapply(data, function(d) {
        if (is.numeric(d)) {
            'numeric'
        } else if (is.factor(d)) {
            'factor'
        } else if (is.character(d)) {
            if (anyNA(suppressWarnings(col2rgb(d)))) {
                'character'
            } else {
                'colour'
            }
        } else if (inherits(d, 'Date')) {
            'date'
        } else if (inherits(d, 'POSIXt')) {
            'datetime'
        } else {
            'constant'
        }
    })
    names(classes) <- names(data)
    classes
}

prepareTween <- function(data, n, ease) {
    if (!is.list(data)) {
        data <- as.list(data)
    }
    if (length(unique(lengths(data))) != 1) {
        stop('All elements in data must have the same length')
    }
    if (!all(ease %in% validEase)) {
        stop('ease must be the name of a valid easing function')
    }
    n <- c(rep(n, length.out = length(data) - 1) - 1, 0)
    ease <- c(rep(ease, length.out = length(data) - 1), 'constant')
    states <- data.frame(
        state = seq_along(data) - 1,
        nframes = n,
        ease = ease,
        stringsAsFactors = FALSE
    )
    list(
        data = data,
        states = states
    )
}
