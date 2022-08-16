#' Generator internals
#'
#' @param x A generator object
#'
#' @return Various data
#'
#' @name gen_internal
#' @rdname gen_internal
#'
#' @keywords internal
#'
NULL

#' @rdname gen_internal
#' @export
PHASE_LEVELS <- c("raw", "static", "transition", "enter", "exit")

#' @rdname gen_internal
#' @export
is_generator <- function(x) inherits(x, "frame_generator")

#' @rdname gen_internal
#' @export
generator_settings <- function(x) attr(x, "generator_settings")

#' @rdname gen_internal
#' @export
`generator_settings<-` <- function(x, value) {
  attr(x, "generator_settings") <- value
  x
}

#' @rdname gen_internal
#' @export
ease_type <- function(x) generator_settings(x)$ease_type

#' @rdname gen_internal
#' @export
`ease_type<-` <- function(x, value) {
  generator_settings(x)$ease_type <- value
  x
}

#' @rdname gen_internal
#' @export
col_types <- function(x) generator_settings(x)$col_types

#' @rdname gen_internal
#' @export
`col_types<-` <- function(x, value) {
  generator_settings(x)$col_types <- value
  x
}

#' @rdname gen_internal
#' @export
gen_data <- function(x) generator_settings(x)$data

#' @rdname gen_internal
#' @export
`gen_data<-` <- function(x, value) {
  generator_settings(x)$data <- value
  x
}

convert_generator <- function(x) {
  UseMethod('convert_generator')
}
#' @export
convert_generator.default <- function(x) {
  list(data = x, settings = list(converter = function(x, ...) x))
}

#' @rdname gen_internal
#' @export
gen_to_data_frame <- function(...) {
  data <- lapply(list(...), convert_generator)
  settings <- lapply(data, `[[`, 'settings')
  data <- lapply(data, `[[`, 'data')
  id <- rep(seq_along(data), vapply(data, nrow, integer(1)))
  data <- vec_rbind(data)
  attr(data, "generator_id") <- id
  attr(data, "generator_info") <- settings
  class(data) <- c("generator_df", class(data))
  data
}

#' @rdname gen_internal
#' @export
data_frame_to_gen <- function(x) {
  if (!inherits(x, "generator_df")) {
    stop('This does not appear to be a generator in data frame disguise')
  }
  info <- attr(x, "generator_info")
  data <- split(data.frame(x), attr(x, "generator_id"))
  Map(function(data, info) {
    info$converter(data, info)
  })
}
