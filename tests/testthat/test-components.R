context("components")

df <- data.frame(
  x = c(1, 5, 7, 10),
  y = c(4, 3, 7, -1),
  col = c('black', 'red', 'green', 'blue'),
  type = letters[1:4],
  stringsAsFactors = FALSE
)

test_that("tween_components works", {
  tween <- tween_components(df, 'linear', nframes = 10, time = c(1, 7, 13, 20))
  expect_equal(nrow(tween), 10)
  expect_equal(tween$x[6], 19/3)
  expect_equal(tween$col[2], '#52170B')
  expect_equal(max(tween$.frame), 10)
  expect_true(all(tween$.phase[c(1,4,7,10)] == 'raw'))
  expect_true(all(tween$.phase[-c(1,4,7,10)] == 'transition'))

  tween <- tween_components(df, 'linear', nframes = 10, time = c(1, 7, 13, 20), rep(c(1,2), 2))
  expect_equal(nrow(tween), 14)
  expect_equal(tween$x[12], 25/3)
  expect_equal(tween$col[2], '#162A10')
  expect_equal(max(tween$.frame), 10)
  expect_true(all(tween$.phase[c(1,5,10,14)] == 'raw'))
  expect_true(all(tween$.phase[-c(1,5,10,14)] == 'transition'))
})

test_that("enter/exit works", {
  tween <- tween_components(df, 'linear', nframes = 20, time = c(1, 7, 13, 20), enter = function(df) {
    df$x <- 0
    df$col <- 'red'
    df
  }, enter_length = 3)
  expect_equal(nrow(tween), 20)
  expect_equal(tween$x[3], 2/3, tolerance = 1e-7)
  expect_equal(tween$col[2], '#A51B0B')
  expect_equal(max(tween$.frame), 20)
  expect_true(all(tween$.phase[1:3] == 'enter'))
})

test_that("weird input gets caught", {
  tween <- tween_components(df, 'linear', nframes = 0, time = c(1, 7, 13, 20))
  expect_equal(nrow(tween), 0)
  tween <- tween_components(df[integer(), ], 'linear', nframes = 10, time = numeric())
  expect_equal(nrow(tween), 0)
  expect_error(tween_components(df, 'linear', nframes = 10, time = 1))
  expect_error(tween_components(df, 'linear', nframes = 0, time = c(1, 7, 13, 20), id = 1))
})
