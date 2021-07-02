#' Generator for interpolating between two data frames
#'
#' This is a generator version of [tween_at()] with the additional functionality
#' of supporting enter and exit functions. It returns a generator that can be
#' used with [get_frame()] and [get_raw_frames()] to extract frames for a
#' specific time point scaled between 0 and 1.
#'
#' @inheritParams tween_at
#' @inheritParams tween_state
#'
#' @return A `keyframe_generator` object
#'
#' @family Other generators
#'
#' @export
#'
#' @examples
#' gen <- gen_at(mtcars[1:6, ], mtcars[6:1, ], 'cubic-in-out')
#'
#' get_frame(gen, 0.3)
gen_at <- function(from, to, ease, id = NULL, enter = NULL, exit = NULL) {
  gen <- gen_keyframe(from, 0)
  add_keyframe(gen, to, ease = ease, length = 1, id = {{ id }}, enter = enter, exit = exit)
}
