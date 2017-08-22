context("rowAlls")

test_that("DMS has equal output to mS", {
  value <- 1
  expecteds <- lapply(unlist(list_of_matrix, recursive = FALSE),
                      matrixStats::rowAlls, value = value)
  lapply(list_of_DelayedMatrix, function(list_of_objects) {
    objects <- unlist(list_of_objects, recursive = FALSE)
    expecteds <- expecteds[match(names(objects), names(expecteds))]
    mapply(function(object, expected) {
      expect_equal(rowAlls(object, value = value), expected)
    }, object = objects, expected = expecteds)
  })
})

test_that("DMS has equal output to mS: subsetting and delayed ops", {
  value <- 1
  i <- c(3, 2)
  j <- c(1, 3)
  f <- function(x) log(x * 3 + 8)
  expecteds <- lapply(list_of_matrix_base_case,
                      function(x) {
                        matrixStats::rowAlls(f(x[i, j]), value = value)
                      })
  lapply(list_of_DelayedMatrix_base_case, function(list_of_objects) {
    objects <- unlist(list_of_objects, recursive = FALSE)
    mapply(function(object, expected) {
      expect_equal(rowAlls(f(object[i, j]), value = value), expected)
    }, object = objects, expected = expecteds)
  })
})

test_that("DMS has equal output to mS: non-NULL rows and cols", {
  value <- 1
  rows <- c(3, 2)
  cols <- c(1, 3)
  expecteds <- lapply(list_of_matrix_base_case,
                      function(x) matrixStats::rowAlls(x, rows, cols, value))
  lapply(list_of_DelayedMatrix_base_case, function(list_of_objects) {
    objects <- unlist(list_of_objects, recursive = FALSE)
    mapply(function(object, expected) {
      expect_equal(rowAlls(object, rows, cols, value), expected)
    }, object = objects, expected = expecteds)
  })
})

# TODO: Test with transposing
# TODO: When there's an error, want to report class of object and expected,
#       possibly even str(), to make it easier to figure out which test failed
