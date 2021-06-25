context("fill")

df <- data.frame(
  x = c(1, NA, NA, NA, 6, 4, NA, NA, NA, NA, 20),
  type = c('a', NA, NA, NA, 'b', 'c', NA, NA, NA, NA, 'd'),
  col = c('red', NA, NA, NA, 'blue', 'green', NA, NA, NA, NA, 'black'),
  stringsAsFactors = FALSE
)
test_that("tween_fill works", {
  tween <- tween_fill(df, 'linear')
  expect_equal(dim(df), dim(tween))
  expect_equal(tween$x[3], 3.5)
  expect_equal(tween$col[10], '#183112')

  expect_equal(tween_fill(df$col, 'linear'), tween$col)

  tween <- tween_fill(df[-c(1, 11), 1], 'linear')
  expect_equal(tween, df$x[-c(1, 11)])
})
