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

# ------------------------------------------------------------------------------
# Unit test functions
#

testDefaultArgs <- function(matrix_list, DelayedMatrix_list) {
  test_that("Default arguments", {
    check.attributes <- checkAttributes(DelayedMatrix_list)
    expecteds <- Map(ms_f, matrix_list)
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
      f = ms_f,
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

# ------------------------------------------------------------------------------
# Run unit tests
#

for (i in seq_len(nrow(test_manifest))) {

  # Get the function to be tested
  f <- test_manifest[i, "Function"]
  # TODO: This is a clunky hack to get matrixStats::f; what's the proper way?
  ms_f <- get(f, envir = environment(sum2))
  dms_f <- match.fun(f)
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
}

# TODO: Test with transposing
# TODO: Test other arguments
# TODO: When there's an error, want to report class of object and expected,
#       possibly even str(), to make it easier to figure out which test failed
# TODO: When `test_integer`, `test_double` or `test_logical` is FALSE, check
#       that an error really occurs if matrixStats::foo() is given that input
# TODO: Add column to manifest that allows all a functions tests to be skipped