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
