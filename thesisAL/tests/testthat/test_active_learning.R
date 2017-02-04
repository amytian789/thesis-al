context("Test active learning")

## .active_learning is correct

test_that("active_learning works", {
  set.seed(10)
  X <- rbind(MASS::mvrnorm(10, rep(0,2), diag(2)),
             MASS::mvrnorm(10, rep(2,2), diag(2)))
  y <- as.factor(c(rep(0, 5), rep(NA, 5), rep(1, 5), rep(NA, 5)))

  res <- active_learning(X, y, method = "lda")

  expect_true(is.numeric(res))
  expect_true(res %% 1 == 0)
  expect_true(res >= 1)
  expect_true(res <= nrow(X))
  expect_true(res %in% which(is.na(y)))
})
