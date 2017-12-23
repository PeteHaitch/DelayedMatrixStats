context("rowTabulates")

test_that("DMS has equal output to mS", {
  # TODO: Remove comment around test that fails due to bug in rowTabulates()
  #       from  CRAN version of matrixStats (0.52.2)
  # # NOTE: matrixStats::rowTabulates() doesn't work with numeric matrices
  # expecteds <- lapply(unlist(list_of_matrix, recursive = FALSE), function(x) {
  #   try(matrixStats::rowTabulates(x), silent = TRUE)
  # })
  # lapply(list_of_DelayedMatrix,
  #        function(list_of_objects) {
  #          objects <- unlist(list_of_objects, recursive = FALSE)
  #          expecteds <- expecteds[match(names(objects), names(expecteds))]
  #          mapply(function(object, expected) {
  #            if (type(object) != "integer") {
  #              # NOTE: matrixStats::rowTabulates() doesn't work with numeric
  #              #       matrices, so should produce an error
  #              expect_error(rowTabulates(object))
  #            } else {
  #              expect_equal(rowTabulates(object), expected)
  #            }
  #          }, object = objects, expected = expecteds)
  #        })

  values <- c(1, 10)
  # NOTE: matrixStats::rowTabulates() doesn't work with numeric matrices, so
  #       only test integer matrices
  expecteds <- lapply(unlist(list_of_matrix, recursive = FALSE), function(x) {
    try(matrixStats::rowTabulates(x, values = values), silent = TRUE)
  })
  lapply(list_of_DelayedMatrix,
         function(list_of_objects) {
           objects <- unlist(list_of_objects, recursive = FALSE)
           expecteds <- expecteds[match(names(objects), names(expecteds))]
           mapply(function(object, expected) {
             if (type(object) != "integer") {
               # NOTE: matrixStats::rowTabulates() doesn't work with numeric
               #       matrices, so should produce an error
               expect_error(rowTabulates(object, values = values))
             } else {
               expect_equal(rowTabulates(object, values = values), expected)
             }
           }, object = objects, expected = expecteds)
         })
})

test_that("DMS has equal output to mS: subsetting and delayed ops", {
  i <- c(3, 2)
  j <- c(1, 3)
  f <- function(x) x * 3L + 8L
  expecteds <- lapply(list_of_matrix_base_case, function(x) {
    try(matrixStats::rowTabulates(f(x[i, j])),
        silent = TRUE)
  })
  lapply(list_of_DelayedMatrix_base_case, function(list_of_objects) {
    objects <- unlist(list_of_objects, recursive = FALSE)
    mapply(function(object, expected) {
      if (type(f(object[i, j])) != "integer") {
        # NOTE: matrixStats::rowTabulates() doesn't work with numeric
        #       matrices, so should produce an error
        expect_error(rowTabulates(f(object[i, j])))
      } else {
        expect_equal(rowTabulates(f(object[i, j])), expected)
      }
    }, object = objects, expected = expecteds)
  })

  values <- c(1, 10)
  expecteds <- lapply(list_of_matrix_base_case, function(x) {
    try(matrixStats::rowTabulates(f(x[i, j]), values = values),
        silent = TRUE)
  })
  lapply(list_of_DelayedMatrix_base_case, function(list_of_objects) {
    objects <- unlist(list_of_objects, recursive = FALSE)
    mapply(function(object, expected) {
      if (type(f(object[i, j])) != "integer") {
        # NOTE: matrixStats::rowTabulates() doesn't work with numeric
        #       matrices, so should produce an error
        expect_error(rowTabulates(f(object[i, j])))
      } else {
        expect_equal(rowTabulates(f(object[i, j]), values = values), expected)
      }
    }, object = objects, expected = expecteds)
  })
})

test_that("DMS has equal output to mS: non-NULL rows and cols", {
  rows <- c(3, 2)
  cols <- c(1, 3)
  expecteds <- lapply(list_of_matrix_base_case, function(x) {
    try(matrixStats::rowTabulates(x, rows, cols), silent = TRUE)
  })
  lapply(list_of_DelayedMatrix_base_case, function(list_of_objects) {
    objects <- unlist(list_of_objects, recursive = FALSE)
    mapply(function(object, expected) {
      if (type(object) != "integer") {
        # NOTE: matrixStats::rowTabulates() doesn't work with numeric
        #       matrices, so should produce an error
        expect_error(rowTabulates(object, rows, cols))
      } else {
        expect_equal(rowTabulates(object, rows, cols), expected)
      }
    }, object = objects, expected = expecteds)
  })

  values <- c(1, 10)
  expecteds <- lapply(list_of_matrix_base_case, function(x) {
    try(matrixStats::rowTabulates(x, rows, cols, values), silent = TRUE)
  })
  lapply(list_of_DelayedMatrix_base_case, function(list_of_objects) {
    objects <- unlist(list_of_objects, recursive = FALSE)
    mapply(function(object, expected) {
      if (type(object) != "integer") {
        # NOTE: matrixStats::rowTabulates() doesn't work with numeric
        #       matrices, so should produce an error
        expect_error(rowTabulates(object, rows, cols, values))
      } else {
        expect_equal(rowTabulates(object, rows, cols, values), expected)
      }
    }, object = objects, expected = expecteds)
  })
})

# TODO: Test with transposing
# TODO: When there's an error, want to report class of object and expected,
#       possibly even str(), to make it easier to figure out which test failed
