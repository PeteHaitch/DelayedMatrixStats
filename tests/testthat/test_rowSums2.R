#-------------------------------------------------------------------------------
# Non-exported methods
#

context("rowSums2")

test_that("rowSums2() conforms to matrixStats API", {
  lapply(seed_types, function(seed_type) {
    # NOTE: Only test those for which object exists (e.g. there is no 'empty'
    #       data.frame seed, so want to skip that)
    objects <- unlist(x = list_of_seeds[[seed_type]], recursive = FALSE)
    expecteds <- unlist(x = list_of_seeds[["matrix"]], recursive = FALSE)
    expecteds <- expecteds[match(names(objects), names(expecteds))]
    mapply(function(object, expected) {
      expect_equal(rowSums2(object), rowSums2(expected))
    }, object = objects, expected = expecteds)
  })
})

#-------------------------------------------------------------------------------
# Exported methods
#

test_that("Identical output", {
  expecteds <- lapply(unlist(list_of_seeds[["matrix"]], recursive = FALSE),
                      matrixStats::rowSums2)
  lapply(list_of_DelayedMatrix, function(list_of_objects) {
    objects <- unlist(list_of_objects, recursive = FALSE)
    expecteds <- expecteds[match(names(objects), names(expecteds))]
    mapply(function(object, expected) {
      expect_equal(rowSums2(object), expected)
    }, object = objects, expected = expecteds)
  })
})

test_that("Identical output with subsetting and delayed ops", {
  i <- c(3, 2)
  j <- c(1, 3)
  f <- function(x) log(x * 3 + 8)
  expecteds <- lapply(list_of_matrix_base_case,
                      function(x) matrixStats::rowSums2(f(x[i, j])))
  lapply(list_of_DelayedMatrix_base_case, function(list_of_objects) {
    objects <- unlist(list_of_objects, recursive = FALSE)
    mapply(function(object, expected) {
      expect_equal(rowSums2(f(object[i, j])), expected)
    }, object = objects, expected = expecteds)
  })
})

test_that("Identical output with nrow and nrow", {
  i <- c(3, 2)
  j <- c(1, 3)
  expecteds <- lapply(list_of_matrix_base_case,
                      function(x) matrixStats::rowSums2(x, i, j))
  lapply(list_of_DelayedMatrix_base_case, function(list_of_objects) {
    objects <- unlist(list_of_objects, recursive = FALSE)
    mapply(function(object, expected) {
      expect_equal(rowSums2(object, i, j), expected)
    }, object = objects, expected = expecteds)
  })
})

# TODO: Test with transposing
# TODO: When there's an error, want to report class of object and expected,
#       possibly even str(), to make it easier to figure out which test failed
