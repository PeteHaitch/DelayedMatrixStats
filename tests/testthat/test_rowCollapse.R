context("rowCollapse")

test_that("DMS has equal output to mS", {
  idxs <- 1
  expecteds <- lapply(unlist(list_of_matrix, recursive = FALSE),
                      matrixStats::rowCollapse, idxs = idxs)
  lapply(list_of_DelayedMatrix, function(list_of_objects) {
    objects <- unlist(list_of_objects, recursive = FALSE)
    expecteds <- expecteds[match(names(objects), names(expecteds))]
    mapply(function(object, expected) {
      expect_equal(rowCollapse(object, idxs = idxs), expected)
    }, object = objects, expected = expecteds)
  })
})

test_that("DMS has equal output to mS: subsetting and delayed ops", {
  idxs <- 1
  i <- c(3, 2)
  f <- function(x) log(x * 3 + 8)
  expecteds <- lapply(list_of_matrix_base_case,
                      function(x) {
                        matrixStats::rowCollapse(f(x[i, ]), idxs = idxs)
                      })
  lapply(list_of_DelayedMatrix_base_case, function(list_of_objects) {
    objects <- unlist(list_of_objects, recursive = FALSE)
    mapply(function(object, expected) {
      expect_equal(rowCollapse(f(object[i, ]), idxs = idxs), expected)
    }, object = objects, expected = expecteds)
  })
})

test_that("DMS has equal output to mS: non-NULL rows", {
  idxs <- 1
  rows <- c(3, 2)
  expecteds <- lapply(list_of_matrix_base_case,
                      function(x) matrixStats::rowCollapse(x, idxs, rows))
  lapply(list_of_DelayedMatrix_base_case, function(list_of_objects) {
    objects <- unlist(list_of_objects, recursive = FALSE)
    mapply(function(object, expected) {
      expect_equal(rowCollapse(object, idxs, rows), expected)
    }, object = objects, expected = expecteds)
  })
})

# TODO: Test with transposing
# TODO: When there's an error, want to report class of object and expected,
#       possibly even str(), to make it easier to figure out which test failed
