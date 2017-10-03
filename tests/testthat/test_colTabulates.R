context("colTabulates")

test_that("DMS has equal output to mS", {
  # NOTE: matrixStats::colTabulates() doesn't work with numeric matrices
  expecteds <- lapply(unlist(list_of_matrix, recursive = FALSE), function(x) {
    try(matrixStats::colTabulates(x), silent = TRUE)
  })
  lapply(list_of_DelayedMatrix,
         function(list_of_objects) {
           objects <- unlist(list_of_objects, recursive = FALSE)
           expecteds <- expecteds[match(names(objects), names(expecteds))]
           mapply(function(object, expected) {
             if (type(object) != "integer") {
               # NOTE: matrixStats::colTabulates() doesn't work with numeric
               #       matrices, so should produce an error
               expect_error()
             } else {
               expect_equal(colTabulates(object), expected)
             }
           }, object = objects, expected = expecteds)
         })

  values <- c(1, 10)
  # NOTE: matrixStats::colTabulates() doesn't work with numeric matrices, so
  #       only test integer matrices
  expecteds <- lapply(unlist(list_of_matrix, recursive = FALSE), function(x) {
    try(matrixStats::colTabulates(x, values = values), silent = TRUE)
  })
  lapply(list_of_DelayedMatrix,
         function(list_of_objects) {
           objects <- unlist(list_of_objects, recursive = FALSE)
           expecteds <- expecteds[match(names(objects), names(expecteds))]
           mapply(function(object, expected) {
             if (type(object) != "integer") {
               # NOTE: matrixStats::colTabulates() doesn't work with numeric
               #       matrices, so should produce an error
               expect_error()
             } else {
               expect_equal(colTabulates(object, values = values), expected)
             }
           }, object = objects, expected = expecteds)
         })
})

test_that("DMS has equal output to mS: subsetting and delayed ops", {
  i <- c(3, 2)
  j <- c(1, 3)
  f <- function(x) x * 3L + 8L
  expecteds <- lapply(list_of_matrix_base_case, function(x) {
    try(matrixStats::colTabulates(f(x[i, j])),
        silent = TRUE)
  })
  lapply(list_of_DelayedMatrix_base_case, function(list_of_objects) {
    objects <- unlist(list_of_objects, recursive = FALSE)
    mapply(function(object, expected) {
      if (type(f(object[i, j])) != "integer") {
        # NOTE: matrixStats::colTabulates() doesn't work with numeric
        #       matrices, so should produce an error
        expect_error()
      } else {
        expect_equal(colTabulates(f(object[i, j])), expected)
      }
    }, object = objects, expected = expecteds)
  })

  values <- c(1, 10)
  expecteds <- lapply(list_of_matrix_base_case, function(x) {
    try(matrixStats::colTabulates(f(x[i, j]), values = values),
        silent = TRUE)
  })
  lapply(list_of_DelayedMatrix_base_case, function(list_of_objects) {
    objects <- unlist(list_of_objects, recursive = FALSE)
    mapply(function(object, expected) {
      if (type(f(object[i, j])) != "integer") {
        # NOTE: matrixStats::colTabulates() doesn't work with numeric
        #       matrices, so should produce an error
        expect_error()
      } else {
        expect_equal(colTabulates(f(object[i, j]), values = values), expected)
      }
    }, object = objects, expected = expecteds)
  })
})

test_that("DMS has equal output to mS: non-NULL rows and cols", {
  rows <- c(3, 2)
  cols <- c(1, 3)
  expecteds <- lapply(list_of_matrix_base_case, function(x) {
    try(matrixStats::colTabulates(x, rows, cols), silent = TRUE)
  })
  lapply(list_of_DelayedMatrix_base_case, function(list_of_objects) {
    objects <- unlist(list_of_objects, recursive = FALSE)
    mapply(function(object, expected) {
      if (type(object) != "integer") {
        # NOTE: matrixStats::colTabulates() doesn't work with numeric
        #       matrices, so should produce an error
        expect_error()
      } else {
        expect_equal(colTabulates(object, rows, cols), expected)
      }
    }, object = objects, expected = expecteds)
  })

  values <- c(1, 10)
  expecteds <- lapply(list_of_matrix_base_case, function(x) {
    try(matrixStats::colTabulates(x, rows, cols, values), silent = TRUE)
  })
  lapply(list_of_DelayedMatrix_base_case, function(list_of_objects) {
    objects <- unlist(list_of_objects, recursive = FALSE)
    mapply(function(object, expected) {
      if (type(object) != "integer") {
        # NOTE: matrixStats::colTabulates() doesn't work with numeric
        #       matrices, so should produce an error
        expect_error()
      } else {
        expect_equal(colTabulates(object, rows, cols, values), expected)
      }
    }, object = objects, expected = expecteds)
  })
})

# TODO: Test with transposing
# TODO: When there's an error, want to report class of object and expected,
#       possibly even str(), to make it easier to figure out which test failed
