context("at")

df1 <- data.frame(x = 1:2, y = 4:5, col = 'black', type = letters[1:2], stringsAsFactors = FALSE)
df2 <- data.frame(x = 11:12, y = 14:15, col = 'white', type = letters[1], stringsAsFactors = FALSE)

test_that("tween_at works", {
  tween <- tween_at(df1, df2, 0.5, 'linear')
  expect_equal(nrow(tween), 2)
  expect_named(tween, names(df1))
  expect_equal(tween$x, c(6, 7))
  expect_equal(tween$col[1], '#767676FF')
})

test_that("tween_at handles weird input", {
  tween <- tween_at(df1, df2[1,], 0.5, 'linear')
  expect_equal(nrow(tween), 2)
  tween <- tween_at(df1[1,], df2, 0.5, 'linear')
  expect_equal(nrow(tween), 2)
  tween <- tween_at(df1, df2[integer(),], 0.5, 'linear')
  expect_equal(nrow(tween), 0)
  tween <- tween_at(df1[integer(),], df2, 0.5, 'linear')
  expect_equal(nrow(tween), 0)
  expect_error(tween_at(df1[c(1,2,1), ], df2, 0.5, 'linear'))
  expect_error(tween_at(df1, df2, numeric(), 'linear'))
  expect_error(tween_at(df1, df2, 0.5, character()))
})

test_that('tween_at works with vectors', {
  tween <- tween_at(df1$x, df2$x, 0.5, 'linear')
  expect_is(tween, 'numeric')
  expect_equal(tween, c(6,7))
  expect_error(tween_at(df1$x, df2$col))
})
