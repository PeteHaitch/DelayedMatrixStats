### =============================================================================
### Utility functions
###

# ------------------------------------------------------------------------------
# Non-exported functions
#

get_verbose <- function() {
  getOption("DelayedMatrixStats.verbose", default = FALSE)
}

set_verbose <- function(verbose) {
  if (!isTRUEorFALSE(verbose)) {
    stop("'verbose' must be TRUE or FALSE")
  }
  old_verbose <- get_verbose()
  options(DelayedMatrixStats.verbose = verbose)
  old_verbose
}

message2 <- function(msg, verbose = FALSE) {
  if (verbose) {
    message(msg)
  }
}

# TODO: Figure out a minimal definition of a "simple seed"; Hervé defines a
#       "seed contract" as dim(), dimnames(), and subset_seed_as_array()
#       https://github.com/Bioconductor-mirror/DelayedArray/blob/18685ee33442b9b8e499a75bd46451c56383f18b/R/cbind-methods.R#L49
#       A potential minimal (albeit almost circular) definition is it has a
#       subset_simple_seed_as_seed_class() method
# TODO: Is an RleArraySeed a simple seed? It's in memory, but doesn't support
#       basic operations like "[", although it does support
#       subset_simple_seed_as_seed_class() and
#       DelayedArray:::subset_seed_as_array(), which may be sufficient
# NOTE: A matterArraySeed is not a simple seed because it does not support
#       subset_simple_seed_as_seed_class
.is_simple_seed <- function(seed) {
  simple_seed_classes <- c("matrix", "Matrix", "data.frame", "DataFrame",
                           "RleArraySeed")
  any(vapply(simple_seed_classes, function(class) is(seed, class), logical(1)))
}

.has_simple_seed <- function(x) {
  .is_simple_seed(seed(x))
}

# NOTE: A basic wrapper around DelayedArray:::.execute_delayed_ops() that also
#       handles seed instance of class RleArraySeed
# TODO: Make generic and implement methods
#' @importFrom S4Vectors endoapply
.execute_delayed_ops <- function(seed, delayed_ops) {
  if (is(seed, "RleArraySeed")) {
    seed@rle <- DelayedArray:::.execute_delayed_ops(seed@rle, delayed_ops)
  } else if (is(seed, "DataFrame")) {
    seed <- endoapply(seed, DelayedArray:::.execute_delayed_ops, delayed_ops)
  } else {
    seed <- DelayedArray:::.execute_delayed_ops(seed, delayed_ops)
  }
  seed
}

# NOTE: Named to avoid clash with base::.subset
# NOTE: Helper function used within [col|row]* functions
..subset <- function(x, rows = NULL, cols = NULL) {
  if (!is.null(rows) && !is.null(cols)) {
    x <- x[rows, cols, drop = FALSE]
  } else if (!is.null(rows)) {
    x <- x[rows, , drop = FALSE] # nolint
  } else if (!is.null(cols)) {
    x <- x[, cols, drop = FALSE]
  }
  x
}

#' Coerce DelayedArray to its 'simple seed' form
#' @details Like `DelayedArray:::.from_DelayedArray_to_array` but returning an
#' object of the same class as `class(seed(x))` instead of an _array_. In
#' doing so, all delayed operations are realised (including subsetting)
#' @param x A \linkS4class{DelayedArray}
#' @param drop If `TRUE` the result is coerced to the lowest possible dimension
#' @param do_transpose Should transposed input be physically transposed?
#'
#' @note Can be more efficient to leave the transpose implicit
#' (`do_transpose = FALSE`) and switch from a `row*()` method to a `col*()`
#' method (or vice versa).
#'
#' @note Only works on \linkS4class{DelayedArray} objects with 'simple seeds'
#'
#' @importFrom S4Vectors isTRUEorFALSE
#' @keywords internal
from_DelayedArray_to_simple_seed_class <- function(x, drop = FALSE,
                                                   do_transpose = TRUE) {
  stopifnot(is(x, "DelayedArray"))
  if (!.is_simple_seed(seed(x))) {
    stop("x does not have a simple seed")
  }
  if (!isTRUEorFALSE(drop)) {
    stop("'drop' must be TRUE or FALSE")
  }
  ans <- subset_simple_seed_as_seed_class(seed(x), unname(x@index))
  # TODO: Doesn't work for certain types of seed; does this matter? (am I going
  #       to have transposed DelayedArray objects coming through this routine?)
  # TODO: Need a dim,RleArraySeed-method
  if (!is.data.frame(ans) && !is(ans, "RleArraySeed") &&
      !is(ans, "DataFrame")) {
    dim(ans) <- DelayedArray:::.get_DelayedArray_dim_before_transpose(x)
  }
  ans <- .execute_delayed_ops(ans, x@delayed_ops)
  # TODO: Need a dimnames,RleArraySeed-method
  if (!is(ans, "RleArraySeed")) {
    dimnames(ans) <-
      DelayedArray:::.get_DelayedArray_dimnames_before_transpose(x)
  }
  if (drop) {
    ans <- DelayedArray:::.reduce_array_dimensions(ans)
  }
  # NOTE: Base R doesn't support transposition of an array of arbitrary
  #       dimension (generalized transposition) so the call to t() below will
  #       fail if 'ans' has more than 2 dimensions. If we want as.array() to
  #       work on a transposed DelayedArray object of arbitrary dimension, we
  #       need to implement our own generalized transposition of an ordinary
  #       array [NOTE copied from DelayedArray]
  if (x@is_transposed && do_transpose) {
    if (length(dim(ans)) > 2L) {
      stop("can't do as.array() on this object, sorry")
    }
    ans <- t(ans)
  }
  ans
}

# Convert a Nindex of a matrix-like object to an IRangesList. Each element of
# the IRangesList corresponds to a column of the matrix-like object and the
# IRanges elements correspond to rows of the matrix-like object
# NOTE: This is typically used to construct the ranges in a RleViews object
#       on a RleArraySeed. This RleViews object then provides efficient ways
#       to compute summaries of the RleArraySeed via Views summary functions
#' @importFrom IRanges IRanges IRangesList PartitioningByEnd
#' @importFrom methods as
#' @importFrom S4Vectors new2
get_Nindex_as_IRangesList <- function(Nindex, dim) {
  stopifnot(is.list(Nindex), is.integer(dim), length(Nindex) ==
              length(dim), length(Nindex) == 2L)
  rows <- Nindex[[1L]]
  cols <- Nindex[[2L]]
  nrow <- dim[[1L]]
  ncol <- dim[[2L]]
  if (ncol == 0) {
    return(IRanges::IRangesList())
  }
  # TODO: Sanity check rows and cols are compatible with dim(seed)

  # Convert rows and cols to IRangesList
  # Four cases
  if (is.null(rows) && is.null(cols)) {
    # Case 1: NULL rows and NULL cols
    ir <- IRanges::IRanges(start = seq.int(1, nrow * ncol, nrow),
                           end = seq.int(nrow, nrow * ncol, nrow))
    partitioning <- IRanges::PartitioningByEnd(seq_len(ncol))
  } else if (is.null(rows) && !is.null(cols)) {
    # Case 2: NULL rows and non-NULL cols
    ir <- IRanges::IRanges(start = (cols - 1L) * nrow + 1L,
                           end = cols * nrow)
    partitioning <- IRanges::PartitioningByEnd(seq_along(cols))
  } else if (!is.null(rows)) {
    ir0 <- as(rows, "IRanges")
    if (is.null(cols)) {
      # Case 3: Non-NULL rows and NULL cols
      start <- vapply(X = seq.int(1, ncol),
                      FUN = function(jj) {
                        start(ir0) + (jj - 1L) * nrow
                      },
                      FUN.VALUE = integer(length(ir0)))
      end <- vapply(X = seq.int(1, ncol),
                    FUN = function(jj) {
                      end(ir0) + (jj - 1L) * nrow
                    },
                    FUN.VALUE = integer(length(ir0)))
      ir <- IRanges::IRanges(start, end)
      partitioning <- IRanges::PartitioningByEnd(
        seq.int(length(ir0), length(ir0) * ncol, length(ir0)))
    } else if (!is.null(cols)) {
      # Case 4: Non-NULL rows and non_NULL cols
      start <- vapply(X = as.integer(cols),
                      FUN = function(jj) {
                        start(ir0) + (jj - 1L) * nrow
                      },
                      FUN.VALUE = integer(length(ir0)))
      end <- vapply(X = as.integer(cols),
                    FUN = function(jj) {
                      end(ir0) + (jj - 1L) * nrow
                    },
                    FUN.VALUE = integer(length(ir0)))
      ir <- IRanges::IRanges(start, end)
      partitioning <- IRanges::PartitioningByEnd(
        seq.int(length(ir0), length(ir0) * length(cols), length(ir0)))
    }
  }
  # TODO: Better way to instantiate the result?
  new2("CompressedIRangesList",
       unlistData = ir,
       partitioning = partitioning)
}

# ------------------------------------------------------------------------------
# Non-exported methods
#

setMethod("subset_simple_seed_as_seed_class", "matrix",
          function(seed, index) {
            DelayedArray:::subset_by_Nindex(x = seed, Nindex = index)
          }
)

setMethod("subset_simple_seed_as_seed_class", "Matrix",
          function(seed, index) {
            DelayedArray:::subset_by_Nindex(x = seed, Nindex = index)
          }
)

# TODO: See https://github.com/Bioconductor-mirror/DelayedArray/blob/229050e7ac587b4e25a0ad0595d69a301b6314a0/R/DelayedArray-class.R#L612
setMethod("subset_simple_seed_as_seed_class", "data.frame",
          function(seed, index) {
            DelayedArray:::subset_by_Nindex(x = seed, Nindex = index)
          }
)

# TODO: See https://github.com/Bioconductor-mirror/DelayedArray/blob/229050e7ac587b4e25a0ad0595d69a301b6314a0/R/DelayedArray-class.R#L630
setMethod("subset_simple_seed_as_seed_class", "DataFrame",
          function(seed, index) {
            DelayedArray:::subset_by_Nindex(x = seed, Nindex = index)
          }
)

# TODO: Might be able to simplify to DelayedArray:::subset_by_Nindex() if
#       `[`,RleArraySeed-method is defined, e.g., via
#       DelayedArray:::to_linear_index() like in the below
setMethod("subset_simple_seed_as_seed_class", "SolidRleArraySeed",
          function(seed, index) {
            seed_dim <- dim(seed)
            i <- DelayedArray:::to_linear_index(Nindex = index,
                                                dim = seed_dim)
            rle <- seed@rle[i]
            dim <- DelayedArray:::get_Nindex_lengths(Nindex = index,
                                                     dim = seed_dim)
            # TODO: Need to subset dimnames and pass to constructor
            dimnames <- list(
              DelayedArray:::get_Nindex_names_along(Nindex = index,
                                                    dimnames = seed@dimnames,
                                                    along = 1L),
              DelayedArray:::get_Nindex_names_along(Nindex = index,
                                                    dimnames = seed@dimnames,
                                                    along = 2L))
            DelayedArray:::RleArraySeed(rle, dim, dimnames)
          }
)

# TODO: subset_simple_seed_as_seed_class,ChunkedRleArraySeed-method
#       (see subset_seed_as_array,ChunkedRleArraySeed-method)
