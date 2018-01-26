context("colLogSumExps")

test_that("DMS has equal output to mS", {
  # NOTE: Don't test integer matrices, which aren't supported by
  #       matrixStats::colLogSumExps()
  list_of_matrix[["integer"]] <- NULL
  list_of_DelayedMatrix <- lapply(list_of_DelayedMatrix, function(x) {
    x[["integer"]] <- NULL
    x
  })
  expecteds <- lapply(unlist(list_of_matrix, recursive = FALSE),
                      matrixStats::colLogSumExps)
  lapply(list_of_DelayedMatrix, function(list_of_objects) {
    objects <- unlist(list_of_objects, recursive = FALSE)
    expecteds <- expecteds[match(names(objects), names(expecteds))]
    mapply(function(object, expected) {
      expect_equal(colLogSumExps(object), expected,
                   check.names = !is(object, "HDF5Array"))
    }, object = objects, expected = expecteds)
  })
})

test_that("DMS has equal output to mS: subsetting and delayed ops", {
  # NOTE: Don't test integer matrices, which aren't supported by
  #       matrixStats::colLogSumExps()
  list_of_matrix[["integer"]] <- NULL
  list_of_DelayedMatrix <- lapply(list_of_DelayedMatrix, function(x) {
    x[["integer"]] <- NULL
    x
  })
  i <- c(3, 2)
  j <- c(1, 3)
  f <- function(x) log(x * 3 + 8)
  expecteds <- lapply(list_of_matrix_base_case,
                      function(x) matrixStats::colLogSumExps(f(x[i, j])))
  lapply(list_of_DelayedMatrix_base_case, function(list_of_objects) {
    objects <- unlist(list_of_objects, recursive = FALSE)
    mapply(function(object, expected) {
      expect_equal(colLogSumExps(f(object[i, j])), expected,
                   check.names = !is(object, "HDF5Array"))
    }, object = objects, expected = expecteds)
  })
})

# TODO: This test fails due to a bug in matrixStats::colLogSumExps(); reported
#       in https://github.com/HenrikBengtsson/matrixStats/issues/120
test_that("DMS has equal output to mS: non-NULL rows and cols", {
  matrixStats_test_case <- try(
    colLogSumExps(list_of_matrix_base_case[["double"]],
                  rows = c(3, 2),
                  cols = c(1, 3)),
    silent = TRUE)
  if (is(matrixStats_test_case, "try-error")) {
    skip("Skipping due to bug in matrixStats::rowLogSumExps() (https://github.com/HenrikBengtsson/matrixStats/issues/120)")
  }

  # NOTE: Drop integer matrices, which aren't supported by
  #       matrixStats::colLogSumExps()
  list_of_matrix_base_case[["integer"]] <- NULL
  list_of_DelayedMatrix_base_case <-
    lapply(list_of_DelayedMatrix_base_case, function(x) {
      x[["integer"]] <- NULL
      x
    })
  rows <- c(3, 2)
  cols <- c(1, 3)
  expecteds <- lapply(list_of_matrix_base_case,
                      function(x) matrixStats::colLogSumExps(x, rows, cols))
  lapply(list_of_DelayedMatrix_base_case, function(list_of_objects) {
    objects <- unlist(list_of_objects, recursive = FALSE)
    mapply(function(object, expected) {
      expect_equal(colLogSumExps(object, rows, cols), expected,
                   check.names = !is(object, "HDF5Array"))
    }, object = objects, expected = expecteds)
  })
})

# TODO: Test with transposing
# TODO: When there's an error, want to report class of object and expected,
#       possibly even str(), to make it easier to figure out which test failed
