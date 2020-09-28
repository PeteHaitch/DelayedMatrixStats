### ============================================================================
### colAvgsPerRowSet
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_colAvgsPerRowSet <- function(X, W = NULL, cols = NULL, S,
                                                  FUN = colMeans, ...,
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
  X <- ..subset(X, cols = cols)

  # Compute result
  # NOTE: Can't use rowblock_APPLY() because it may process as few as 1 row per
  #       iteration (and colAvgsPerRowSet() may require access to multiple rows)
  val <- lapply(seq_len(ncol(S)), function(k) {
    if (!is.null(W)) {
      W <- W[, k]
    }
    matrixStats::colAvgsPerRowSet(as.matrix(X[S[, k], , drop = FALSE]),
                                  W = W,
                                  S = matrix(seq_len(nrow(S))),
                                  FUN = FUN,
                                  tFUN = tFUN)
  })
  if (length(val) == 0L) {
    return(matrix(numeric(nrow(X)), nrow = nrow(X)))
  }
  # NOTE: Return value of matrixStats::colAvgsPerRowSet() has rownames
  do.call(rbind, val)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit MatrixGenerics::colAvgsPerRowSet
#' @importMethodsFrom DelayedArray seed
#' @rdname colAvgsPerRowSet
#' @template common_params
#' @template uppercase_X
#' @export
#' @template example_dm_S4VectorsDF
#' @author Peter Hickey
#' @examples
#' colAvgsPerRowSet(dm_DF, S = matrix(1:2, ncol = 2))
setMethod("colAvgsPerRowSet", "DelayedMatrix",
          function(X, W = NULL, cols = NULL, S, FUN = colMeans, ...,
                   force_block_processing = FALSE, tFUN = FALSE) {
            .smart_seed_dispatcher(X, generic = "colAvgsPerRowSet", 
                                   blockfun = .DelayedMatrix_block_colAvgsPerRowSet,
                                   force_block_processing = force_block_processing,
                                   W = W,
                                   cols = cols,
                                   S = S,
                                   FUN = FUN,
                                   ...,
                                   tFUN = tFUN)
          }
)
