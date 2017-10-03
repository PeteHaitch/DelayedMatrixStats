### ============================================================================
### rowAvgsPerColSet
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

#' `rowAvgsPerColSet()` block-processing internal helper
#' @inherit matrixStats::rowAvgsPerColSet
#' @importFrom methods is
.DelayedMatrix_block_rowAvgsPerColSet <- function(X, W = NULL, rows = NULL, S,
                                                  FUN = rowMeans, ...,
                                                  tFUN = FALSE) {
  # Check input type
  stopifnot(is(X, "DelayedMatrix"))
  if (is(W, "DelayedMatrix")) {
    warning("'W' will be realised in-memory as a matrix")
    W <- as.matrix(W)
  }
  if (is(S, "DelayedMatrix")) {
    warning("'S' will be realised in-memory as a matrix")
    S <- as.matrix(S)
  }
  stopifnot(!X@is_transposed)
  DelayedArray:::.get_ans_type(X)

  # Subset
  X <- ..subset(X, rows = rows)

  # Compute result
  # NOTE: Can't use DelayedArray:::colblock_APPLY() because it may process as
  #       few as 1 column per iteration (and rowAvgsPerColSet() may require
  #       access to multiple columns)
  val <- lapply(seq_len(ncol(S)), function(j) {
    if (!is.null(W)) {
      W <- W[, j]
    }
    matrixStats::rowAvgsPerColSet(as.matrix(X[, S[, j]]),
                                  W = W,
                                  S = matrix(seq_len(nrow(S))),
                                  FUN = FUN,
                                  tFUN = tFUN)
  })
  if (length(val) == 0L) {
    return(matrix(numeric(ncol(X)), ncol = ncol(X)))
  }
  # NOTE: Return value of matrixStats::rowAvgsPerColSet() has rownames
  do.call(cbind, val)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @importFrom DelayedArray seed
#' @importFrom methods hasMethod is
#' @rdname colAvgsPerRowSet
#' @export
setMethod("rowAvgsPerColSet", "DelayedMatrix",
          function(X, W = NULL, rows = NULL, S, FUN = colMeans, ...,
                   force_block_processing = FALSE, tFUN = FALSE) {
            if (!hasMethod("rowAvgsPerColSet", class(seed(X))) ||
                force_block_processing) {
              message2("Block processing", get_verbose())
              return(.DelayedMatrix_block_rowAvgsPerColSet(X = X,
                                                           W = W,
                                                           rows = rows,
                                                           S = S,
                                                           FUN = FUN,
                                                           ...,
                                                           tFUN = tFUN))
            }

            message2("Has seed-aware method", get_verbose())
            if (DelayedArray:::is_pristine(X)) {
              message2("Pristine", get_verbose())
              simple_seed_X <- seed(X)
            } else {
              message2("Coercing to seed class", get_verbose())
              # TODO: do_transpose trick
              simple_seed_X <- try(from_DelayedArray_to_simple_seed_class(X),
                                   silent = TRUE)
              if (is(simple_seed_X, "try-error")) {
                message2("Unable to coerce to seed class", get_verbose())
                return(rowAvgsPerColSet(X = X,
                                        W = W,
                                        rows = rows,
                                        S = S,
                                        FUN = FUN,
                                        ...,
                                        force_block_processing = TRUE,
                                        tFUN = tFUN))
              }
            }

            rowAvgsPerColSet(X = simple_seed_X,
                             W = W,
                             rows = rows,
                             S = S,
                             FUN = FUN,
                             ...,
                             tFUN = tFUN)
          }
)

# ------------------------------------------------------------------------------
# Seed-aware methods
#

#' @importFrom methods setMethod
#' @export
setMethod("rowAvgsPerColSet", "matrix", matrixStats::rowAvgsPerColSet)
