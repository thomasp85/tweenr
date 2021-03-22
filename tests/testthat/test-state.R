context("state")

df1 <- data.frame(x = 1:2, y = 4:5, col = 'black', type = letters[1:2], stringsAsFactors = FALSE)
df2 <- data.frame(x = 11:12, y = 14:15, col = 'white', type = letters[1], stringsAsFactors = FALSE)

test_that("tween_state works", {
  tween <- tween_state(df1, df2, ease = 'linear', nframes = 5)
  expect_equal(max(tween$.frame), 5)
  expect_true(all(tween$.phase[c(1:2, 9:10)] == 'raw'))
  expect_true(all(tween$.phase[c(3:8)] == 'transition'))
  expect_true(all(tween$.id == rep(1:2, 5)))
  expect_equal(tween$col[5], '#767676FF')
  expect_equal(tween$x[7], 8.5)
  expect_equal(tween$type[4:5], c('b', 'a'))
})

test_that("keep_state works", {
  expect_warning(
    keep <- keep_state(df1, 5),
    NA
  )
  expect_equal(max(keep$.frame), 5)
  expect_true(all(keep$.phase[c(9:10)] == 'raw'))
  expect_true(all(keep$.phase[c(1:8)] == 'static'))
})

test_that("enter/exit works", {
  tween <- tween_state(df1, df2[1,, drop = FALSE], 'linear', 5, exit = function(df) {
    df$x <- 0
    df$col <- 'red'
    df
  })
  expect_equal(nrow(tween), 9)
  expect_true(all(tween$.phase[c(4,6,8)] == 'exit'))
  expect_equal(tween$col[8], '#BA1808FF')
  expect_equal(tween$x[8], 0.5)
})
