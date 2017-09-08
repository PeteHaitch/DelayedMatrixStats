context("rowAvgsPerColSet.R")

test_that("DMS has equal output to mS", {
  S <- matrix(c(1, 4), nrow = 1, ncol = 2)
  expecteds <- lapply(unlist(list_of_matrix, recursive = FALSE),
                      function(x) {
                        matrixStats::rowAvgsPerColSet(
                          x,
                          S = S[, seq_len(min(ncol(x), ncol(S))), drop = FALSE])
                      })
  lapply(list_of_DelayedMatrix, function(list_of_objects) {
    objects <- unlist(list_of_objects, recursive = FALSE)
    expecteds <- expecteds[match(names(objects), names(expecteds))]
    mapply(function(object, expected) {
      expect_equal(rowAvgsPerColSet(
        object,
        S = S[, seq_len(min(ncol(object), ncol(S))),  drop = FALSE]),
        expected,
        check.attributes = !is(object, "HDF5Array") &&
          all(dim(object) != c(0, 0)))
    }, object = objects, expected = expecteds)
  })
})

test_that("DMS has equal output to mS: subsetting and delayed ops", {
  S <- matrix(c(1, 4))
  i <- c(2, 1)
  f <- function(x) log(x * 3 + 8)
  expecteds <- lapply(list_of_matrix_base_case,
                      function(x) {
                        matrixStats::rowAvgsPerColSet(
                          f(x[i, , drop = FALSE]),
                          S = S[, seq_len(min(ncol(x[i, , drop = FALSE]),
                                              ncol(S))), drop = FALSE])
                      })
  lapply(list_of_DelayedMatrix_base_case, function(list_of_objects) {
    objects <- unlist(list_of_objects, recursive = FALSE)
    mapply(function(object, expected) {
      expect_equal(rowAvgsPerColSet(
        f(object[i, , drop = FALSE]),
        S = S[, seq_len(min(ncol(object[i, , drop = FALSE]), ncol(S))),
              drop = FALSE]),
        expected,
        check.attributes = !is(object, "HDF5Array") &&
          all(dim(object) != c(0, 0)))
    }, object = objects, expected = expecteds)
  })
})

test_that("DMS has equal output to mS: non-NULL cols", {
  S <- matrix(c(1, 4))
  rows <- c(2, 1)
  expecteds <- lapply(list_of_matrix_base_case,
                      function(x) {
                        matrixStats::rowAvgsPerColSet(
                          x,
                          rows = rows,
                          S = S[, seq_len(min(ncol(x), ncol(S))), drop = FALSE])
                      })
  lapply(list_of_DelayedMatrix_base_case, function(list_of_objects) {
    objects <- unlist(list_of_objects, recursive = FALSE)
    mapply(function(object, expected) {
      expect_equal(
        rowAvgsPerColSet(object,
                         rows = rows,
                         S = S[, seq_len(min(ncol(object), ncol(S))),
                               drop = FALSE]),
        expected,
        check.attributes = !is(object, "HDF5Array") &&
          all(dim(object) != c(0, 0)))
    }, object = objects, expected = expecteds)
  })
})

# TODO: Test with transposing
# TODO: When there's an error, want to report class of object and expected,
#       possibly even str(), to make it easier to figure out which test failed
# TODO: Test with different `W`, `cols`, `FUN`, `tFUN`
