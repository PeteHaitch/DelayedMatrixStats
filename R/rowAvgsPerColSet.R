### ============================================================================
### rowAvgsPerColSet
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_rowAvgsPerColSet <- function(X, W = NULL, rows = NULL, S,
                                                  FUN = rowMeans, ...,
                                                  na.rm = NA,
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
  DelayedArray:::.get_ans_type(X, must.be.numeric = TRUE)

  # Subset
  X <- ..subset(X, rows = rows)

  # Compute result
  # NOTE: Can't use DelayedArray:::colblock_APPLY() because it may process as
  #       few as 1 column per iteration (and rowAvgsPerColSet() may require
  #       access to multiple columns)
  val <- lapply(seq_len(ncol(S)), function(k) {
    if (!is.null(W)) {
      W <- W[, k]
    }
    rowAvgsPerColSet(as.matrix(X[, S[, k], drop = FALSE]),
                     W = W,
                     S = matrix(seq_len(nrow(S))),
                     FUN = FUN,
                     na.rm = na.rm,
                     tFUN = tFUN)
  })
  if (length(val) == 0L) {
    return(matrix(numeric(ncol(X)), ncol = ncol(X)))
  }
  do.call(cbind, val)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit MatrixGenerics::rowAvgsPerColSet
#' @importMethodsFrom DelayedArray seed
#' @rdname colAvgsPerRowSet
#' @export
#' @examples
#'
#' rowAvgsPerColSet(dm_DF, S = matrix(1:2, ncol = 1))
setMethod("rowAvgsPerColSet", "DelayedMatrix",
          function(X, W = NULL, rows = NULL, S, FUN = rowMeans, ...,
                   force_block_processing = FALSE, na.rm = NA, tFUN = FALSE) {
            .smart_seed_dispatcher(X, generic = MatrixGenerics::rowAvgsPerColSet,
                                   blockfun = .DelayedMatrix_block_rowAvgsPerColSet,
                                   force_block_processing = force_block_processing,
                                   W = W,
                                   rows = rows,
                                   S = S,
                                   FUN = FUN,
                                   ...,
                                   na.rm = na.rm,
                                   tFUN = tFUN)
          }
)
