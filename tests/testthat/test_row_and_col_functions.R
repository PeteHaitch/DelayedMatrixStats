### ============================================================================
### Load manifest.
###

# NOTE: The manifest lists which unit tests are to be applied to each function
test_manifest <- read.csv(
  file = "test_manifest.csv",
  stringsAsFactors = FALSE)

# ------------------------------------------------------------------------------
# Helper functions
#

filterMatrixList <- function(list, test_integer, test_double, test_logical) {
  if (!test_integer) {
    list <- list[!grepl("integer", names(list))]
  }
  if (!test_double) {
    list <- list[!grepl("double", names(list))]
  }
  if (!test_logical) {
    list <- list[!grepl("logical", names(list))]
  }
  list
}

# Should dimnames be checked? There are reasons for not checking these:
# - A HDF5Matrix cannot have dimnames
# - A 0x0 DelayedMatrix may have dimnames set as `NULL` or `list(NULL, NULL)`
checkAttributes <- function(DelayedMatrix_list) {
  Map(
    f = function(x) !is(x, "HDF5Matrix") && !identical(dim(x), c(0L, 0L)),
    DelayedMatrix_list)
}

expect_equal_DA <- function(object, expected, ...) {
  expect_equal(as.matrix(object), as.matrix(expected), ...)
}

# ------------------------------------------------------------------------------
# Unit test functions
#

testDefaultArgs <- function(matrix_list, DelayedMatrix_list) {
  test_that("Default arguments", {
    check.attributes <- checkAttributes(DelayedMatrix_list)
    expecteds <- Map(mg_f, matrix_list)
    observeds <- Map(dms_f, DelayedMatrix_list)
    Map(expect_equal, observeds, expecteds, check.attributes = check.attributes)
  })
}

testNonNullRowsAndCols <- function(matrix_list, DelayedMatrix_list) {
  test_that("Non-NULL rows and cols", {
    # NOTE: Only run these tests on 'base_case'
    matrix_list <- matrix_list[grep("base_case", names(matrix_list))]
    DelayedMatrix_list <- DelayedMatrix_list[
      grep("base_case", names(DelayedMatrix_list))]
    check.attributes <- checkAttributes(DelayedMatrix_list)
    rows_list <- list(c(3, 2))
    cols_list <- list(c(1, 3))
    expecteds <- Map(
      f = mg_f,
      matrix_list,
      rows = rows_list,
      cols = cols_list)
    observeds <- Map(
      f = dms_f,
      DelayedMatrix_list,
      rows = rows_list,
      cols = cols_list)
    Map(expect_equal, observeds, expecteds, check.attributes = check.attributes)
  })
}

testGroup <- function(matrix_list, DelayedMatrix_list) {
  test_that("Default arguments (group)", {
    # NOTE: Only run these tests on 'base_case'
    matrix_list <- matrix_list[grep("base_case", names(matrix_list))]
    DelayedMatrix_list <- DelayedMatrix_list[
      grep("base_case", names(DelayedMatrix_list))]
    check.attributes <- checkAttributes(DelayedMatrix_list)
    if (any(grepl("col", body(dms_f)))) {
      group_list <- list(c(1, 2, 2, 2))
    } else {
      group_list <- list(c(1, 1, 2))
    }
    expecteds <- Map(mg_f, matrix_list, group = group_list)
    observeds <- Map(dms_f, DelayedMatrix_list, group = group_list)
    Map(expect_equal_DA, observeds, expecteds,
        check.attributes = check.attributes)
  })
}

# ------------------------------------------------------------------------------
# Run unit tests
#

for (i in seq_len(nrow(test_manifest))) {

  # Get the function to be tested
  f <- test_manifest[i, "Function"]

  # This hack is to preserve the `useNames = FALSE` behaviour of
  # colAnyMissings/rowAnyMissings in BioC <= BioC 3.17 and matrixStats < 1.0.0
  # in BioC == 3.17 with matrixStats == 1.0.0.
  # colAnyMissings/rowAnyMissings are effectively aliases for
  # colAnyNAs/rowAnyNAs and will be deprecated in the next release.
  if (f == "colAnyMissings") {
    f <- "colAnyNAs"
  } else if (f == "rowAnyMissings") {
    f <- "rowAnyNAs"
  }
  # A clunky hack to get MatrixGenerics::f
  mg_f <- get(f, envir = environment(MatrixGenerics::colSums2))
  dms_f <- get(f)
  context(f)

  # Filter out those storage modes not to be tested
  test_integer <- test_manifest[i, "test_integer"]
  test_double <- test_manifest[i, "test_double"]
  test_logical <- test_manifest[i, "test_logical"]
  filtered_matrix_list <- filterMatrixList(
    list = matrix_list,
    test_integer = test_integer,
    test_double = test_double,
    test_logical = test_logical)
  filtered_DelayedMatrix_list <- filterMatrixList(
    list = DelayedMatrix_list,
    test_integer = test_integer,
    test_double = test_double,
    test_logical = test_logical)

  # Run selected tests
  if (test_manifest[i, "testDefaultArgs"]) {
    testDefaultArgs(filtered_matrix_list, filtered_DelayedMatrix_list)
  }
  if (test_manifest[i, "testNonNullRowsAndCols"]) {
    testNonNullRowsAndCols(filtered_matrix_list, filtered_DelayedMatrix_list)
  }
  if (test_manifest[i, "testGroup"]) {
    testGroup(filtered_matrix_list, filtered_DelayedMatrix_list)
  }
}

# TODO: Test with transposing
# TODO: Test other arguments
# TODO: When there's an error, want to report class of object and expected,
#       possibly even str(), to make it easier to figure out which test failed
# TODO: When `test_integer`, `test_double` or `test_logical` is FALSE, check
#       that an error really occurs if matrixStats::foo() is given that input
# TODO: Add column to manifest that allows all a functions tests to be skipped
