context("along")

df <- data.frame(
  x = c(1, 5, 7, 10),
  y = c(4, 3, 7, -1),
  col = c('black', 'red', 'green', 'blue'),
  type = letters[1:4],
  stringsAsFactors = FALSE
)

test_that("tween_along works", {
  tween <- tween_along(df, ease = 'linear', nframes = 10, along = x)
  expect_equal(nrow(tween), 30)
  expect_equal(tween$col[22], '#78B785')
  expect_equal(tween$y[8], 3.25)

  tween <- tween_along(df, ease = 'linear', nframes = 10, along = x, history = FALSE)
  expect_equal(nrow(tween), 9)
  expect_equal(tween$col[8], '#78B785')
  expect_equal(tween$y[2], 3.75)
})

test_that("tween_along throws errors", {
  expect_error(tween_along(df, ease = 'linear', nframes = 10, along = 1))
  expect_error(tween_along(df, ease = 'linear', nframes = 10, along = x, id = 1))
  expect_error(tween_along(df, ease = 'linear', nframes = 10, along = x, range = c(0, 0)))
  expect_error(tween_along(df[1,], ease = 'linear', nframes = 10, along = x))
})
