context("GitHub issues")

test_that("Issue 54 is fixed", {
  # NOTE: This is a slow-ish test, so don't test when running tests with small
  #       block size.
  skip_if(getAutoBlockSize() == 8)

  # Small normal matrix
  m1 <- DelayedArray(as.matrix(iris[, 1:4]))
  expect_equal(
    rowsum(as.matrix(m1), iris$Species),
    rowsum(m1, iris$Species, force_block_processing = FALSE))
  expect_equal(
    rowsum(m1, iris$Species, force_block_processing = FALSE),
    rowsum(m1, iris$Species, force_block_processing = TRUE))

  # Large sparse matrix
  x <- Matrix::rsparsematrix(800000, ncol = 50, density = 0.1)

  # Large normal matrix
  m2 <- DelayedArray(as.matrix(x))
  S <- sample(1:1000, nrow(m2), replace = TRUE)
  expect_equal(
    rowsum(as.matrix(m2), S),
    rowsum(m2, S, force_block_processing = FALSE))
  expect_equal(
    rowsum(as.matrix(m2), S),
    rowsum(m2, S, force_block_processing = TRUE))

  # RleMatrix
  m3 <- as(m2, "RleMatrix")
  S <- sample(1:1000, nrow(m3), replace = TRUE)
  expect_equal(
    rowsum(as.matrix(m3), S),
    rowsum(m3, S, force_block_processing = FALSE)
  )
  expect_equal(
    rowsum(as.matrix(m3), S),
    rowsum(m3, S, force_block_processing = TRUE)
  )

  # dgCMatrix
  m4 <- DelayedArray(x)
  S <- sample(1:1000, nrow(m4), replace = TRUE)
  expect_equal(
    rowsum(as.matrix(m4), S),
    rowsum(m4, S, force_block_processing = FALSE))
  expect_equal(
    rowsum(as.matrix(m4), S),
    rowsum(m4, S, force_block_processing = TRUE))
})
