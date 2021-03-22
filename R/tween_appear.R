#' Tween a data.frame of appearances
#'
#' This function is intended for use when you have a data.frame of events at
#' different time points. This could be the appearance of an observation for
#' example. This function replicates your data `nframes` times and
#' calculates the duration of each frame. At each frame each row is
#' assigned an age based on the progression of frames and the entry point of in
#' time for that row. A negative age means that the row has not appeared yet.
#'
#' @param data A data.frame to tween
#'
#' @param time The name of the column that holds the time dimension. This does
#' not need to hold time data in the strictest sence - any numerical type will
#' do
#'
#' @param timerange The range of time to create the tween for. If missing it
#' will defaults to the range of the time column
#'
#' @param nframes The number of frames to create for the tween. If missing it
#' will create a frame for each full unit in `timerange` (e.g.
#' `timerange = c(1, 10)` will give `nframes = 10`)
#'
#' @return A data.frame as `data` but repeated `nframes` times and
#' with the additional columns `.age` and `.frame`
#'
#' @family data.frame tween
#'
#' @examples
#' data <- data.frame(
#'   x = rnorm(100),
#'   y = rnorm(100),
#'   time = sample(50, 100, replace = TRUE)
#' )
#'
#' data <- tween_appear(data, 'time', nframes = 200)
#'
#' @export
#'
tween_appear <- function(data, time, timerange, nframes) {
  if (missing(timerange) || is.null(timerange)) {
    timerange <- range(data[[time]])
  }
  if (missing(nframes) || is.null(nframes)) {
    nframes <- ceiling(diff(timerange) + 1)
  }
  framelength <- diff(timerange) / nframes
  frametimes <- seq(timerange[1], timerange[2], length.out = nframes)

  tweendata <- lapply(seq_along(frametimes), function(i) {
    data$.age <- frametimes[i] - data[[time]]
    data$.frame <- i
    data
  })
  tweendata <- do.call(rbind, tweendata)
  attr(tweendata, 'framelength') <- framelength
  tweendata
}
