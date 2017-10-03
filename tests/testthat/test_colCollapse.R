context("colCollapse")

test_that("DMS has equal output to mS", {
  idxs <- 1
  expecteds <- lapply(unlist(list_of_matrix, recursive = FALSE),
                      matrixStats::colCollapse, idxs = idxs)
  lapply(list_of_DelayedMatrix, function(list_of_objects) {
    objects <- unlist(list_of_objects, recursive = FALSE)
    expecteds <- expecteds[match(names(objects), names(expecteds))]
    mapply(function(object, expected) {
      expect_equal(colCollapse(object, idxs = idxs), expected)
    }, object = objects, expected = expecteds)
  })
})

test_that("DMS has equal output to mS: subsetting and delayed ops", {
  idxs <- 1
  j <- c(1, 3)
  f <- function(x) log(x * 3 + 8)
  expecteds <- lapply(list_of_matrix_base_case,
                      function(x) {
                        matrixStats::colCollapse(f(x[, j]), idxs = idxs)
                      })
  lapply(list_of_DelayedMatrix_base_case, function(list_of_objects) {
    objects <- unlist(list_of_objects, recursive = FALSE)
    mapply(function(object, expected) {
      expect_equal(colCollapse(f(object[, j]), idxs = idxs), expected)
    }, object = objects, expected = expecteds)
  })
})

test_that("DMS has equal output to mS: non-NULL cols", {
  idxs <- 1
  cols <- c(1, 3)
  expecteds <- lapply(list_of_matrix_base_case,
                      function(x) matrixStats::colCollapse(x, idxs, cols))
  lapply(list_of_DelayedMatrix_base_case, function(list_of_objects) {
    objects <- unlist(list_of_objects, recursive = FALSE)
    mapply(function(object, expected) {
      expect_equal(colCollapse(object, idxs, cols), expected)
    }, object = objects, expected = expecteds)
  })
})

# TODO: Test with transposing
# TODO: When there's an error, want to report class of object and expected,
#       possibly even str(), to make it easier to figure out which test failed
