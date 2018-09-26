context("events")

df <- data.frame(
  x = c(1, 5, 7, 10),
  y = c(4, 3, 7, -1),
  col = c('black', 'red', 'green', 'blue'),
  type = letters[1:4],
  stringsAsFactors = FALSE
)

test_that("tween_events works", {
  tween <- tween_events(df, 'linear', 20, x, x + 2)
  expect_equal(nrow(tween), 17)
  expect_equal(max(tween$.frame), 20)
  expect_true(all(tween$.phase[c(4, 8, 13, 17)] == 'raw'))
  expect_true(all(tween$.phase[-c(4, 8, 13, 17)] == 'static'))


  tween <- tween_events(df, 'linear', 20, x, enter = function(df) {
    df$x <- 0
    df$col <- 'red'
    df
  }, enter_length = 3)


  expect_equal(nrow(tween), 23)
  expect_equal(max(tween$.frame), 20)
  expect_true(all(tween$.phase[c(6, 13, 17, 23)] == 'raw'))
  expect_true(all(tween$.phase[-c(6, 13, 17, 23)] == 'enter'))
  expect_equal(tween$x[2], 0.2)
  expect_equal(tween$col[3], '#931B0BFF')
})

test_that("weird input gets handled", {
  expect_error(tween_events(df, 'linear', 20))
  tween <- tween_events(df, 'linear', 0, x)
  expect_equal(nrow(tween), 0)
  tween <- tween_events(df[integer(), ], 'linear', 10, x)
  expect_equal(nrow(tween), 0)
})
