context("colOrderStats")

test_that("DMS has equal output to mS", {
  # NOTE: Drop 0 x 0 matrix (order stats are ill-defined) and those with NA/NaN
  #       (matrixStats::colOrderStats() cannot handle such matrices)
  list_of_matrix <- lapply(list_of_matrix, function(x) {
    x[["empty"]] <- NULL
    i <- grep("NA|NaN", names(x), invert = TRUE)
    x[i]
  })
  list_of_DelayedMatrix <- lapply(list_of_DelayedMatrix, function(x) {
    x <- lapply(x, function(xx) {
      xx[["empty"]] <- NULL
      i <- grep("NA|NaN", names(x), invert = TRUE)
      xx[i]
    })
    x
  })
  which <- 1
  expecteds <- lapply(unlist(list_of_matrix, recursive = FALSE),
                      matrixStats::colOrderStats, which = which)
  lapply(list_of_DelayedMatrix, function(list_of_objects) {
    objects <- unlist(list_of_objects, recursive = FALSE)
    expecteds <- expecteds[match(names(objects), names(expecteds))]
    mapply(function(object, expected) {
      expect_equal(colOrderStats(object, which = which), expected)
    }, object = objects, expected = expecteds)
  })
})

test_that("DMS has equal output to mS: subsetting and delayed ops", {
  which <- 2
  i <- c(3, 2)
  j <- c(1, 3)
  f <- function(x) log(x * 3 + 8)
  expecteds <- lapply(list_of_matrix_base_case,
                      function(x) matrixStats::colOrderStats(f(x[i, j]),
                                                             which = which))
  lapply(list_of_DelayedMatrix_base_case, function(list_of_objects) {
    objects <- unlist(list_of_objects, recursive = FALSE)
    mapply(function(object, expected) {
      expect_equal(colOrderStats(f(object[i, j]), which = which), expected)
    }, object = objects, expected = expecteds)
  })
})

test_that("DMS has equal output to mS: non-NULL rows and cols", {
  which <- 2
  rows <- c(3, 2)
  cols <- c(1, 3)
  expecteds <- lapply(list_of_matrix_base_case,
                      function(x) matrixStats::colOrderStats(x, rows, cols,
                                                             which = which))
  lapply(list_of_DelayedMatrix_base_case, function(list_of_objects) {
    objects <- unlist(list_of_objects, recursive = FALSE)
    mapply(function(object, expected) {
      expect_equal(colOrderStats(object, rows, cols, which = which), expected)
    }, object = objects, expected = expecteds)
  })
})

# TODO: Test with transposing
# TODO: When there's an error, want to report class of object and expected,
#       possibly even str(), to make it easier to figure out which test failed
