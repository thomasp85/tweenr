#' Extract a frame from a generator
#'
#' Using the generators in tweenr you can avoid calculating all needed frames up
#' front, which can be prohibitive in memory. With a generator you can use
#' `get_frame()` to extract any frame at a fractional location between 0 and 1
#' one by one as you need them. You can further get all raw data before and/or
#' after a given point in time using `get_raw_frames()`.
#'
#' @param generator A `frame_generator` object
#' @param at A scalar numeric between 0 and 1
#' @param before,after Scalar numerics that define the time before and after
#' `at` to search for raw data
#' @param ... Arguments passed on to methods
#'
#' @export
#'
#' @examples
#' data <- data.frame(
#'   x = c(1, 2, 2, 1, 2, 2),
#'   y = c(1, 2, 2, 2, 1, 1),
#'   time = c(1, 4, 8, 4, 8, 10),
#'   id = c(1, 1, 1, 2, 2, 2)
#' )
#'
#' gen <- gen_components(data, 'cubic-in-out', time = time, id = id)
#'
#' get_frame(gen, 0.3)
#'
#' get_raw_frames(gen, 0.5, before = 0.5, after = 0.2)
get_frame <- function(generator, at, ...) {
  UseMethod("get_frame")
}
#' @rdname get_frame
#' @export
get_raw_frames <- function(generator, at, before = 0, after = 0, ...) {
  UseMethod("get_raw_frames")
}
