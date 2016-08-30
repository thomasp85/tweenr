# tweenr

[![Travis-CI Build Status](https://travis-ci.org/thomasp85/tweenr.svg?branch=master)](https://travis-ci.org/thomasp85/tweenr)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/tweenr)](http://cran.r-project.org/package=tweenr)
[![CRAN_Download_Badge](http://cranlogs.r-pkg.org/badges/tweenr)](http://cran.r-project.org/package=tweenr)

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
tweenr is available through CRAN using `install.packages('tweenr')`. In order to
get the development version, you can install directly from GitHub using 
devtools:

```r
if (!require(devtools)) {
    install.packages("devtools")
}
devtools::install_github("thomasp85/tweenr")
```

Once you have it there are currently three ways to tween your data. Furthermore
there is also access to standard vectorized interpolaters for the following 
classes:

- numeric
- Date
- POSIXt
- colour (not really a class - any string that can be considered a colour)

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
This function is the most versatile of them all. It takes a data.frame that 
contains a time column, an observation id column and an ease column along with 
the rest of the data. Each observation, defined by the id, is animated through 
its individual states, using its own easing function:

![tween_elements](https://dl.dropboxusercontent.com/u/2323585/tweenr/element.gif)

## How did I make those two animations?
Following are the code needed to recreate the two animations shown above. Most
of the code is concerned with generating the (useless) data, needed for the
animations, the tweenr use is a simple one-liner. Both code samples require the 
use of [ggforce](https://github.com/thomasp85/ggforce), but only because I'm 
lazy... :-)

### The Dancing Ball
```r
library(ggplot2)
library(gganimate)
library(ggforce)
library(tweenr)

# Making up data
t <- data.frame(x=0, y=0, colour = 'forestgreen', size=1, alpha = 1, 
                stringsAsFactors = FALSE)
t <- t[rep(1, 12),]
t$alpha[2:12] <- 0
t2 <- t
t2$y <- 1
t2$colour <- 'firebrick'
t3 <- t2
t3$x <- 1
t3$colour <- 'steelblue'
t4 <- t3
t4$y <- 0
t4$colour <- 'goldenrod'
t5 <- t4
c <- ggforce::radial_trans(c(1,1), c(1, 12))$transform(rep(1, 12), 1:12)
t5$x <- (c$x + 1) / 2
t5$y <- (c$y + 1) / 2
t5$alpha <- 1
t5$size <- 0.5
t6 <- t5
t6 <- rbind(t5[12,], t5[1:11, ])
t6$colour <- 'firebrick'
t7 <- rbind(t6[12,], t6[1:11, ])
t7$colour <- 'steelblue'
t8 <- t7
t8$x <- 0.5
t8$y <- 0.5
t8$size <- 2
t9 <- t
ts <- list(t, t2, t3, t4, t5, t6, t7, t8, t9)

tweenlogo <- data.frame(x=0.5, y=0.5, label = 'tweenr', stringsAsFactors = F)
tweenlogo <- tweenlogo[rep(1, 60),]
tweenlogo$.frame <- 316:375

# Using tweenr
tf <- tween_states(ts, tweenlength = 2, statelength = 1, 
                   ease = c('cubic-in-out', 'elastic-out', 'bounce-out', 
                            'cubic-out', 'sine-in-out', 'sine-in-out', 
                            'circular-in', 'back-out'), 
                   nframes = 375)

# Animate with gganimate
p <- ggplot(data=tf, aes(x=x, y=y)) + 
    geom_text(aes(label = label, frame = .frame), data=tweenlogo, size = 13) + 
    geom_point(aes(frame = .frame, size=size, alpha = alpha, colour = colour)) + 
    scale_colour_identity() + 
    scale_alpha(range = c(0, 1), guide = 'none') +
    scale_size(range = c(4, 60), guide = 'none') + 
    expand_limits(x=c(-0.36, 1.36), y=c(-0.36, 1.36)) + 
    theme_bw()
animation::ani.options(interval = 1/15)
gg_animate(p, "dancing ball.gif", title_frame = F, ani.width = 400, 
           ani.height = 400)
```

### The Dropping Balls
```r
library(ggplot2)
library(gganimate)
library(ggforce)
library(tweenr)

# Making up data
d <- data.frame(x = rnorm(20), y = rnorm(20), time = sample(100, 20), alpha = 0, 
                size = 1, ease = 'elastic-out', id = 1:20, 
                stringsAsFactors = FALSE)
d2 <- d
d2$time <- d$time + 10
d2$alpha <- 1
d2$size <- 3
d2$ease <- 'linear'
d3 <- d2
d3$time <- d2$time + sample(50:100, 20)
d3$size = 10
d3$ease <- 'bounce-out'
d4 <- d3
d4$y <- min(d$y) - 0.5
d4$size <- 2
d4$time <- d3$time + 10
d5 <- d4
d5$time <- max(d5$time)
df <- rbind(d, d2, d3, d4, d5)

# Using tweenr
dt <- tween_elements(df, 'time', 'id', 'ease', nframes = 500)

# Animate with gganimate
p <- ggplot(data = dt) + 
    geom_point(aes(x=x, y=y, size=size, alpha=alpha, frame = .frame)) + 
    scale_size(range = c(0.1, 20), guide = 'none') + 
    scale_alpha(range = c(0, 1), guide = 'none') + 
    ggforce::theme_no_axes()
animation::ani.options(interval = 1/24)
gg_animate(p, 'dropping balls.gif', title_frame = F)
```
