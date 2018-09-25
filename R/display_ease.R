#' Display an easing function
#'
#' This simple helper lets you explore how the different easing functions govern
#' the interpolation of data.
#'
#' @details
#' How transitions proceed between states are defined by an easing function. The
#' easing function converts the parameterized progression from one state to the
#' next to a new number between 0 and 1. `linear` easing is equivalent to
#' an identity function that returns the input unchanged. In addition there are
#' a range of additional easers available, each with three modifiers.
#'
#' \strong{Easing modifiers:}
#' \describe{
#'  \item{-in}{The easing function is applied as-is}
#'  \item{-out}{The easing function is applied in reverse}
#'  \item{-in-out}{The first half of the transition it is applied as-is, while
#'  in the last half it is reversed}
#' }
#'
#' \strong{Easing functions}
#' \describe{
#'  \item{quadratic}{Models a power-of-2 function}
#'  \item{cubic}{Models a power-of-3 function}
#'  \item{quartic}{Models a power-of-4 function}
#'  \item{quintic}{Models a power-of-5 function}
#'  \item{sine}{Models a sine function}
#'  \item{circular}{Models a pi/2 circle arc}
#'  \item{exponential}{Models an exponential function}
#'  \item{elastic}{Models an elastic release of energy}
#'  \item{back}{Models a pullback and relase}
#'  \item{bounce}{Models the bouncing of a ball}
#' }
#'
#' In addition to this function a good animated explanation can be found
#' [here](http://easings.net).
#'
#' @param ease The name of the easing function to display (see details)
#'
#' @return This function is called for its side effects
#'
#' @examples
#' # The default - identity
#' display_ease('linear')
#'
#' # A more fancy easer
#' display_ease('elastic-in')
#'
#' @importFrom graphics plot
#' @export
#'
display_ease <- function(ease) {
  easepoints <- tween_numeric(c(0, 1), 100, ease)[[1]]
  progress <- seq(0, 1, length.out = 100)
  plot(progress, easepoints, type = 'l', main = ease, xlab = 'In',
       ylab = 'Out', bty = 'n')
}
