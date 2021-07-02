PHASE_LEVELS <- c("raw", "static", "transition", "enter", "exit")

is_generator <- function(x) inherits(x, "frame_generator")
generator_settings <- function(x) attr(x, "generator_settings")
`generator_settings<-` <- function(x, value) {
  attr(x, "generator_settings") <- value
  x
}
ease_type <- function(x) generator_settings(x)$ease_type
`ease_type<-` <- function(x, value) {
  generator_settings(x)$ease_type <- value
  x
}
col_types <- function(x) generator_settings(x)$col_types
`col_types<-` <- function(x, value) {
  generator_settings(x)$col_types <- value
  x
}
gen_data <- function(x) generator_settings(x)$data
`gen_data<-` <- function(x, value) {
  generator_settings(x)$data <- value
  x
}
