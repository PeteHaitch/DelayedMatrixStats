context("colWeightedSds")

test_that("DMS has equal output to mS", {
  w <- runif(3)
  expecteds <- lapply(unlist(list_of_matrix, recursive = FALSE),
                      function(x) {
                        matrixStats::colWeightedSds(x, w = w[seq_len(nrow(x))])
                      })
  lapply(list_of_DelayedMatrix, function(list_of_objects) {
    objects <- unlist(list_of_objects, recursive = FALSE)
    expecteds <- expecteds[match(names(objects), names(expecteds))]
    mapply(function(object, expected) {
      expect_equal(colWeightedSds(object, w = w[seq_len(nrow(object))]),
                   expected,
                   check.names = !is(object, "HDF5Array"))
    }, object = objects, expected = expecteds)
  })
})

test_that("DMS has equal output to mS: subsetting and delayed ops", {
  w <- runif(2)
  i <- c(3, 2)
  j <- c(1, 3)
  f <- function(x) log(x * 3 + 8)
  expecteds <- lapply(list_of_matrix_base_case,
                      function(x) {
                        matrixStats::colWeightedSds(
                          f(x[i, j]),
                          w = w[seq_len(nrow(x[i, j]))])
                      })
  lapply(list_of_DelayedMatrix_base_case, function(list_of_objects) {
    objects <- unlist(list_of_objects, recursive = FALSE)
    mapply(function(object, expected) {
      expect_equal(colWeightedSds(f(object[i, j]),
                                   w = w[seq_len(nrow(object[i, j]))]),
                   expected,
                   check.names = !is(object, "HDF5Array"))
    }, object = objects, expected = expecteds)
  })
})

test_that("DMS has equal output to mS: non-NULL rows and cols", {
  w <- runif(3)
  rows <- c(3, 2)
  cols <- c(1, 3)
  expecteds <- lapply(list_of_matrix_base_case,
                      function(x) {
                        matrixStats::colWeightedSds(
                          x,
                          w[seq_len(nrow(x))],
                          rows,
                          cols)
                      })
  lapply(list_of_DelayedMatrix_base_case, function(list_of_objects) {
    objects <- unlist(list_of_objects, recursive = FALSE)
    mapply(function(object, expected) {
      expect_equal(colWeightedSds(object,
                                   w[seq_len(nrow(object))],
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
