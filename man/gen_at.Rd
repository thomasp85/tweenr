% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gen_at.R
\name{gen_at}
\alias{gen_at}
\title{Generator for interpolating between two data frames}
\usage{
gen_at(from, to, ease, id = NULL, enter = NULL, exit = NULL)
}
\arguments{
\item{from, to}{A data.frame or vector of the same type. If either is of
length/nrow 1 it will get repeated to match the length of the other}

\item{ease}{A character vector giving valid easing functions. Recycled to
match the ncol of \code{from}}

\item{id}{The column to match observations on. If \code{NULL} observations will be
matched by position. See the \emph{Match, Enter, and Exit} section for more
information.}

\item{enter, exit}{functions that calculate a start state for new observations
that appear in \code{to} or an end state for observations that are not present in
\code{to}. If \code{NULL} the new/old observations will not be part of the tween. The
function gets a data.frame with either the start state of the exiting
observations, or the end state of the entering observations and must return
a modified version of that data.frame. See the \emph{Match, Enter, and Exit}
section for more information.}
}
\value{
A \code{keyframe_generator} object
}
\description{
This is a generator version of \code{\link[=tween_at]{tween_at()}} with the additional functionality
of supporting enter and exit functions. It returns a generator that can be
used with \code{\link[=get_frame]{get_frame()}} and \code{\link[=get_raw_frames]{get_raw_frames()}} to extract frames for a
specific time point scaled between 0 and 1.
}
\examples{
gen <- gen_at(mtcars[1:6, ], mtcars[6:1, ], 'cubic-in-out')

get_frame(gen, 0.3)
}
\seealso{
Other Other generators: 
\code{\link{gen_along}()},
\code{\link{gen_components}()},
\code{\link{gen_events}()},
\code{\link{gen_keyframe}()}
}
\concept{Other generators}
