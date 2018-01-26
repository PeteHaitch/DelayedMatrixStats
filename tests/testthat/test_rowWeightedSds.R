context("rowWeightedSds")

test_that("DMS has equal output to mS", {
  w <- 1 + runif(4)
  expecteds <- lapply(unlist(list_of_matrix, recursive = FALSE),
                      function(x) {
                        matrixStats::rowWeightedSds(x,
                                                     w = w[seq_len(ncol(x))])
                      })
  lapply(list_of_DelayedMatrix, function(list_of_objects) {
    objects <- unlist(list_of_objects, recursive = FALSE)
    expecteds <- expecteds[match(names(objects), names(expecteds))]
    mapply(function(object, expected) {
      expect_equal(rowWeightedSds(object, w = w[seq_len(ncol(object))]),
                   expected,
                   check.names = !is(object, "HDF5Array"))
    }, object = objects, expected = expecteds)
  })
})

test_that("DMS has equal output to mS: subsetting and delayed ops", {
  w <- 1 + runif(2)
  i <- c(3, 2)
  j <- c(1, 3)
  f <- function(x) log(x * 3 + 8)
  expecteds <- lapply(list_of_matrix_base_case,
                      function(x) {
                        matrixStats::rowWeightedSds(
                          f(x[i, j]),
                          w = w[seq_len(ncol(x[i, j]))])
                      })
  lapply(list_of_DelayedMatrix_base_case, function(list_of_objects) {
    objects <- unlist(list_of_objects, recursive = FALSE)
    mapply(function(object, expected) {
      expect_equal(rowWeightedSds(f(object[i, j]),
                                   w = w[seq_len(ncol(object[i, j]))]),
                   expected,
                   check.names = !is(object, "HDF5Array"))
    }, object = objects, expected = expecteds)
  })
})

test_that("DMS has equal output to mS: non-NULL rows and cols", {
  w <- 1 + runif(4)
  rows <- c(3, 2)
  cols <- c(1, 3)
  expecteds <- lapply(list_of_matrix_base_case,
                      function(x) {
                        matrixStats::rowWeightedSds(
                          x,
                          w[seq_len(ncol(x))],
                          rows,
                          cols)
                      })
  lapply(list_of_DelayedMatrix_base_case, function(list_of_objects) {
    objects <- unlist(list_of_objects, recursive = FALSE)
    mapply(function(object, expected) {
      expect_equal(rowWeightedSds(object,
                                   w[seq_len(ncol(object))],
                                   rows,
                                   cols),
                   expected,
                   check.names = !is(object, "HDF5Array"))
    }, object = objects, expected = expecteds)
  })
})

# TODO: Test with transposing
# TODO: When there's an error, want to report class of object and expected,
#       possibly even str(), to make it easier to figure out which test failed
# TODO: Test with different `na.rm`
