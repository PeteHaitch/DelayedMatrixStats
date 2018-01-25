context("colAvgsPerRowSet.R")

test_that("DMS has equal output to mS", {
  S <- matrix(c(1, 3), nrow = 2, ncol = 1)
  # TODO: Remove -c(2, 8) which is a hack to avoid triggering an error in the
  #       CRAN version of matrixStats (0.52.2)
  expecteds <- lapply(unlist(list_of_matrix, recursive = FALSE)[-c(2, 8)],
                      function(x) {
                        matrixStats::colAvgsPerRowSet(
                          x,
                          S = S[seq_len(min(nrow(x), nrow(S))), , drop = FALSE])
                      })
  lapply(list_of_DelayedMatrix, function(list_of_objects) {
    objects <- unlist(list_of_objects, recursive = FALSE)
    # TODO: Remove next line which is a hack to avoid triggering an error in
    #       the CRAN version of matrixStats (0.52.2)
    objects <- objects[sapply(objects, ncol) > 1]
    expecteds <- expecteds[match(names(objects), names(expecteds))]
    mapply(function(object, expected) {
      expect_equal(colAvgsPerRowSet(
        object,
        S = S[seq_len(min(nrow(object), nrow(S))),  , drop = FALSE]),
        expected,
        check.attributes = !is(object, "HDF5Array") &&
          all(dim(object) != c(0, 0)))
    }, object = objects, expected = expecteds)
  })
})

test_that("DMS has equal output to mS: subsetting and delayed ops", {
  S <- matrix(c(1, 3), nrow = 2, ncol = 1)
  j <- c(2, 1)
  f <- function(x) log(x * 3 + 8)
  expecteds <- lapply(list_of_matrix_base_case,
                      function(x) {
                        matrixStats::colAvgsPerRowSet(
                          f(x[, j, drop = FALSE]),
                          S = S[, seq_len(min(ncol(x[, j, drop = FALSE]),
                                              ncol(S))), drop = FALSE])
                      })
  lapply(list_of_DelayedMatrix_base_case, function(list_of_objects) {
    objects <- unlist(list_of_objects, recursive = FALSE)
    mapply(function(object, expected) {
      expect_equal(colAvgsPerRowSet(
        f(object[, j, drop = FALSE]),
        S = S[seq_len(min(nrow(object[, j, drop = FALSE]), nrow(S))), ,
              drop = FALSE]),
        expected,
        check.attributes = !is(object, "HDF5Array") &&
          all(dim(object) != c(0, 0)))
    }, object = objects, expected = expecteds)
  })
})

test_that("DMS has equal output to mS: non-NULL cols", {
  S <- matrix(c(1, 3), nrow = 2, ncol = 1)
  cols <- c(2, 1)
  expecteds <- lapply(list_of_matrix_base_case,
                      function(x) {
                        matrixStats::colAvgsPerRowSet(
                          x,
                          cols = cols,
                          S = S[, seq_len(min(ncol(x), ncol(S))), drop = FALSE])
                      })
  lapply(list_of_DelayedMatrix_base_case, function(list_of_objects) {
    objects <- unlist(list_of_objects, recursive = FALSE)
    mapply(function(object, expected) {
      expect_equal(
        colAvgsPerRowSet(object,
                         cols = cols,
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
