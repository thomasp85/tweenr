BASEDATE <- Sys.Date() - as.numeric(Sys.Date())
BASEDATETIME <- Sys.time() - as.numeric(Sys.time())

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
