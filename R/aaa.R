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

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @rdname gen_internal
#' @export
#' @importFrom farver decode_colour
col_classes <- function(data) {
  classes <- vapply(data, vec_tween_class, character(1))
  names(classes) <- names(data)
  classes[names(classes) == '.phase'] <- 'phase'
  classes
}

#' Get the nominal class of a vector
#'
#' @param x a vector
#'
#' @export
#' @keywords internal
vec_tween_class <- function(x) {
  UseMethod('vec_tween_class')
}
#' @export
vec_tween_class.default <- function(x) 'constant'
#' @export
vec_tween_class.numeric <- function(x) 'numeric'
#' @export
vec_tween_class.logical <- function(x) 'logical'
#' @export
vec_tween_class.factor <- function(x) 'factor'
#' @export
vec_tween_class.character <- function(x) {
  colour <- try(suppressWarnings(decode_colour(x)), silent = TRUE)
  if (all(is.na(x)) || inherits(colour, 'try-error') || any(is.na(x) != is.na(colour[, 1])) || all(grepl('^(\\d|\\.)+$', x))) {
    'character'
  } else {
    'colour'
  }
}
#' @export
vec_tween_class.Date <- function(x) 'date'
#' @export
vec_tween_class.POSIXt <- function(x) 'datetime'
#' @export
vec_tween_class.list <- function(x) {
  if (all(vapply(x, is.numeric, logical(1)))) 'numlist'
  else 'list'
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
  n <- c(rep(n, length.out = length(data) - 1) - 1, 1)
  ease <- c(rep(ease, length.out = length(data) - 1), 'constant')
  states <- data.frame(
    state = seq_along(data) - 1L,
    nframes = as.integer(n),
    ease = ease,
    stringsAsFactors = FALSE
  )
  list(
    data = data,
    states = states
  )
}

prepareTweenTranspose <- function(data, n, ease) {
  if (!is.list(data)) {
    data <- list(data)
  }
  if (!all(ease %in% validEase)) {
    stop('ease must be the name of a valid easing function')
  }
  n <- rep(n, length.out = length(data))
  n <- Map(function(n, l) {
    s <- floor(n / l)
    s <- rep(s, l)
    overhead <- n - sum(s)
    if (overhead) {
      s <- s + rep(floor(overhead / l), l)
      addInd <- seq_len(n - sum(s))
      s[addInd] <- s[addInd] + 1
    }
    c(s, 1)
  }, n = n - 1, l = lengths(data) - 1)
  n <- unlist(n)
  ease <- rep(ease, length.out = length(data))
  ease <- rep(ease, lengths(data) - 1)
  easeSplit <- split(ease, rep(seq_along(data), lengths(data) - 1))
  ease <- unlist(lapply(easeSplit, append, values = 'constant'))
  data <- as.list(unlist(data))
  states <- data.frame(
    state = seq_along(data) - 1L,
    nframes = as.integer(n),
    ease = ease,
    stringsAsFactors = FALSE
  )
  list(
    data = data,
    states = states
  )
}

first <- function(x) x[[1]]
`first<-` <- function(x, value) {
  x[[1]] <- value
  x
}
last <- function(x) x[[length(x)]]
`last<-` <- function(x, value) {
  x[[length(x)]] <- value
  x
}
