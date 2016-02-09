# tweenr

[![Travis-CI Build Status](https://travis-ci.org/thomasp85/tweenr.svg?branch=master)](https://travis-ci.org/thomasp85/tweenr)

## What is this?
tweenr is a small package that makes it easy to interpolate your data between
different states, specifying the length of each change, the easing of the
transition and how many intermediary steps should be generated. tweenr works
particularly well with [gganimate](https://github.com/dgrtwo/gganimate) but can
be used for any case where interpolation of data is needed. All functions are
vectorized so in any case you'll get better performance than using `approx` and 
`colorRamp`.

![tweenr](https://dl.dropboxusercontent.com/u/2323585/tweenr/showreel.gif)

*tweening of 9 states of data*

## How does it work?
Installation is currently through the use of `devtools` but the package will
eventually land on CRAN:

```r
if (!require(devtools)) {
    install.packages("devtools")
}
devtools::install_github("thomasp85/tweenr")
```

Once you have it there are currently two ways to tween your data (more are
comming):

### tween_states
`tween_states` takes a list of data.frames, each representing a state of your
data, and interpolates the transition between them. Only the first data.frame
needs to be full, the following only needs to contain the columns that shows any
change. It is possible to specify the length of each individual transition, as
well as the length of the pause at each state. Each transition can also have an 
easing function assiciated with it that describes how the transition should 
progress.

### tween_appear
This simple function is for data that describes events in time. It converts the
data into frames and assigns an age to each observation in each frame. A 
negative age means that the observation has yet to appear.

### tween_elements
**TBD** This function is the most versatile of them all. It takes a data.frame
that contains a time column, a observation.id column and an ease column along
with the rest of the data. Each observation, defined by the observation.id, is
animated through its individual states, using its own easing function.
