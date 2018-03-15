#' @details
#' tweenr is a small collection of functions to help you in creating
#' intermediary representations of your data, i.e. interpolating states of data.
#' As such it's a great match for packages such as animate and gganimate, since
#' it can work directly with data.frames of data, but it also provide fast and
#' efficient interpolaters for numeric, date, datetime and colour that are
#' vectorized and thus more efficient to use than the build in interpolation
#' functions (mainly [stats::approx()] and
#' [grDevices::colorRamp()]).
#'
#' The main functions for data.frames are [tween_states()],
#' [tween_elements()] and [tween_appear()], while the
#' standard interpolaters can be found at [tween()]
#'
#' @useDynLib tweenr
#' @importFrom Rcpp sourceCpp
'_PACKAGE'
