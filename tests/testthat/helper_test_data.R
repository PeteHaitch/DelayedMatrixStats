### ============================================================================
### Integer and numeric matrix-like objects used in tests
###

### ----------------------------------------------------------------------------
### List of matrix objects
###

# NOTE: These examples, adapted from those used in unit tests of matrixStats,
#       are chosen to cover various corner cases sometimes encountered when
#       operating on matrices.

modes <- c("integer", "double", "logical")
names(modes) <- modes
# NOTE: Largest matrix has `nrow` rows and `ncol` columns
matrix_list <- unlist(
  x = lapply(modes, function(mode, nrow = 3L, ncol = 4L) {

    # Base case: nrow * ncol matrix with no NA elements and with dimnames
    x <- matrix(
      data = seq(1 - (nrow * ncol) / 2, (nrow * ncol) / 2),
      nrow = nrow,
      ncol = ncol,
      dimnames = list(paste0("R", seq_len(nrow)), paste0("C", seq_len(ncol))))
    storage.mode(x) <- mode

    # Special case: Single-element matrix
    x_1x1 <- x[1, 1, drop = FALSE]

    # Special case: Empty matrix
    x_empty <- x[0, 0]

    # Special case: All NAs
    x_all_NA <- matrix(
      data = NA,
      nrow = nrow,
      ncol = ncol,
      dimnames = list(paste0("R", seq_len(nrow)), paste0("C", seq_len(ncol))))
    storage.mode(x_all_NA) <- mode

    # Special case: All NaNs
    x_all_NaN <- matrix(
      data = NaN,
      nrow = nrow,
      ncol = ncol,
      dimnames = list(paste0("R", seq_len(nrow)), paste0("C", seq_len(ncol))))
    storage.mode(x_all_NaN) <- mode

    # Special case: All NAs and NaNs
    x_all_NA_or_NaN <- matrix(
      data = c(NA, NaN),
      nrow = nrow,
      ncol = ncol,
      dimnames = list(paste0("R", seq_len(nrow)), paste0("C", seq_len(ncol))))
    storage.mode(x_all_NA_or_NaN) <- mode

    if (mode == "double") {
      # Special case: All Inf
      x_all_Inf <- matrix(
        data = Inf,
        nrow = nrow,
        ncol = ncol,
        dimnames = list(paste0("R", seq_len(nrow)), paste0("C", seq_len(ncol))))
      storage.mode(x_all_Inf) <- mode

      # Special case: All -Inf
      x_all_neg_Inf <- matrix(
        data = -Inf,
        nrow = nrow,
        ncol = ncol,
        dimnames = list(paste0("R", seq_len(nrow)), paste0("C", seq_len(ncol))))
      storage.mode(x_all_neg_Inf) <- mode

      # Special case: Inf and -Inf
      x_all_Inf_or_neg_Inf <- matrix(
        data = c(Inf, -Inf),
        nrow = nrow,
        ncol = ncol,
        dimnames = list(paste0("R", seq_len(nrow)), paste0("C", seq_len(ncol))))
      storage.mode(x_all_Inf_or_neg_Inf) <- mode

      val <- list(
        "base_case" = x,
        "1x1" = x_1x1,
        "empty" = x_empty,
        "all_NA" = x_all_NA,
        "all_NaN" = x_all_NaN,
        "all_Inf" = x_all_Inf,
        "all_neg_Inf" = x_all_neg_Inf,
        "all_Inf_or_neg_Inf" = x_all_Inf_or_neg_Inf,
        "all_NA_or_NaN" = x_all_NA_or_NaN)
    } else {
      val <- list(
        "base_case" = x,
        "1x1" = x_1x1,
        "empty" = x_empty,
        "all_NA" = x_all_NA,
        "all_NaN" = x_all_NaN,
        "all_NA_or_NaN" = x_all_NA_or_NaN)
    }
  }),
  recursive = FALSE)

### ----------------------------------------------------------------------------
### List of objects to be used as seeds
###

# NOTE: The classes of objects that can be used as a seed can be divided into
#       two camps:
#       1. Data seeds: These store matrix-like data, where the data is
#       in-memory or on-disk.
#       2. DelayedOp seeds: These store delayed operations.

data_seeds <- c(
  "matrix",
  "Matrix",
  "sparseMatrix",
  "SolidRleArraySeed",
  "ChunkedRleArraySeed",
  "HDF5ArraySeed")
delayed_op_seeds <- c(
  "DelayedSubset",
  "DelayedAperm",
  "DelayedUnaryIsoOp",
  "DelayedDimnames",
  "DelayedNaryIsoOp",
  "DelayedAbind")
seed_classes <- data_seeds
seed_classes <- c(data_seeds, delayed_op_seeds)
names(seed_classes) <- seed_classes

seedFunFactory <- function(seed_class) {
  # NOTE: All DelayedOps are designed to be no-ops
  switch(
    seed_class,
    "matrix" = identity,
    "Matrix" = function(x) {
      if (identical(dim(x), c(1L, 1L))) {
        # NOTE: This is a workaround for a bug in Matrix() that makes all 1x1
        #       matrices into a symmetric matrix while not preserving dimnames.
        if (is.logical(x)) {
          return(as(x, "lgeMatrix"))
        } else {
          return(as(x, "dgeMatrix"))
        }
      }
      Matrix::Matrix(x)
    },
    "sparseMatrix" = function(x) {
      if (identical(dim(x), c(1L, 1L))) {
        # NOTE: This is a workaround for a bug in Matrix() that makes all 1x1
        #       matrices into a symmetric matrix while not preserving dimnames.
        if (is.logical(x)) {
          return(as(x, "lgCMatrix"))
        } else {
          return(as(x, "dgCMatrix"))
        }
      }
      Matrix::Matrix(x, sparse = TRUE)
    },
    "SolidRleArraySeed" = function(x) {
      seed(RleArray(Rle(x), dim(x), dimnames(x), chunksize = NULL))
    },
    "ChunkedRleArraySeed" = function(x) {
      seed(RleArray(Rle(x), dim(x), dimnames(x), chunksize = nrow(x)))
    },
    "HDF5ArraySeed" = function(x) seed(realize(x = x, BACKEND = "HDF5Array")),
    "DelayedSubset" = DelayedArray:::new_DelayedSubset,
    "DelayedAperm" = DelayedArray:::new_DelayedAperm,
    "DelayedUnaryIsoOp" = DelayedArray:::new_DelayedUnaryIsoOp,
    "DelayedDimnames" = function(x) {
      DelayedArray:::new_DelayedDimnames(seed = x, dimnames = dimnames(x))
    },
    "DelayedNaryIsoOp" = DelayedArray:::new_DelayedNaryIsoOp,
    "DelayedAbind" = function(x) {
      if (nrow(x)) {
        seeds <- list(
          x[seq_len(nrow(x) - 1), , drop = FALSE],
          x[nrow(x), , drop = FALSE])
      } else {
        seeds <- list(x, x)
      }
      DelayedArray:::new_DelayedAbind(seeds = seeds, along = 1L)
    }
  )
}

seed_funs <- Map(seedFunFactory, seed_classes)
seed_list <- unlist(
  x = Map(function(f) Map(f, matrix_list), seed_funs),
  recursive = FALSE)

### ----------------------------------------------------------------------------
### List of DelayedMatrix objects
###

# NOTE: Temporarily disable seed simplification to ensure DelayedOps are
#       "simplified away".
options("DelayedArray.simplify" = FALSE)
DelayedMatrix_list <- Map(DelayedArray, seed_list)
options("DelayedArray.simplify" = TRUE)

# ### ----------------------------------------------------------------------------
# ### Check all DelayedMatrix objects match the matrix when as.matrix()
# ###
#
# TODO: Should this be a formal test? Not all tests will pass, e.g.,
#       HDF5ArraySeed don't carry dimnames
# a <- Map(
#   all.equal,
#   Map(as.matrix, DelayedMatrix_list),
#   matrix_list)
# i <- !sapply(a, isTRUE)
# # TODO: Look at all these failures and fix
# names(a[i])
