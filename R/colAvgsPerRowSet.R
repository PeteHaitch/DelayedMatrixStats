### ============================================================================
### colAvgsPerRowSet
###

# ------------------------------------------------------------------------------
# Non-exported methods
#

#' Column averages per rows set of DelayedMatrix using block-processing method
#' @inherit matrixStats::colAvgsPerRowSet
#' @importFrom matrixStats colAvgsPerRowSet
#' @importFrom methods is
.DelayedMatrix_block_colAvgsPerRowSet <- function(X, W = NULL, cols = NULL, S,
                                                  FUN = colMeans,
                                                  tFUN = FALSE,
                                                  ...) {
  stopifnot(is(X, "DelayedMatrix"))
  stopifnot(!X@is_transposed)

  # Check input type
  DelayedArray:::.get_ans_type(X)
  X <- ..subset(X, cols = cols)
  # TODO: Suspect this will break when `S` is missing in the above call
  val <- DelayedArray:::colblock_APPLY(X,
                                       matrixStats::colAvgsPerRowSet,
                                       W = W,
                                       S = S,
                                       FUN = FUN,
                                       tFUN = tFUN,
                                       ...)
  if (length(val) == 0L) {
    return(numeric(ncol(X)))
  }
  # NOTE: Return value of matrixStats::colAnys() has no names
  unlist(val, recursive = FALSE, use.names = FALSE)
}

# ------------------------------------------------------------------------------
# Exported methods
#

#' @importFrom matrixStats colAvgsPerRowSet
#' @importFrom methods setMethod
#' @rdname colAvgsPerRowSet
#' @export
setMethod("colAvgsPerRowSet", "matrix",
          function(X, W = NULL, cols = NULL, S, FUN = colMeans, tFUN = FALSE,
                   ...) {
            message2(class(X), get_verbose())
            matrixStats::colAvgsPerRowSet(X, W, cols, S, FUN, tFUN, ...)
          }
)

#' @importFrom DelayedArray seed
#' @rdname colAvgsPerRowSet
#' @export
setMethod("colAvgsPerRowSet", "DelayedMatrix",
          function(X, W = NULL, cols = NULL, S, FUN = colMeans, tFUN = FALSE,
                   ...) {
            if (X@is_transposed) {
              message2("Transposed", get_verbose())
              stop("Not yet implemented")
              # TODO: How to pass to rowAvgsPerColSet()?
            }
            if (.has_simple_seed(X)) {
              message2("Simple seed", get_verbose())
              if (DelayedArray:::is_pristine(X)) {
                message2("Pristine", get_verbose())
                X <- seed(X)
              } else {
                message2("Coercing to seed class", get_verbose())
                X <- from_DelayedArray_to_simple_seed_class(X)
              }
              return(colAvgsPerRowSet(X, W, cols, S, FUN, tFUN, ...))
            } else {
              message2("Block processing", get_verbose())
              .DelayedMatrix_block_colAvgsPerRowSet(X, W, cols, S, FUN, tFUN,
                                                    ...)
            }
          }
)

# TODO: Additional colAvgsPerRowSet() methods

# TODO: ANY may be too general?
#' @importFrom DelayedArray DelayedArray
#' @importFrom methods setMethod
#' @rdname colAvgsPerRowSet
#' @export
setMethod("colAvgsPerRowSet", "ANY",
          function(X, W = NULL, cols = NULL, S, FUN = colMeans, tFUN = FALSE,
                   ...) {
            message2("ANY", get_verbose())
            X <- DelayedArray::DelayedArray(X)
            .DelayedMatrix_block_colAvgsPerRowSet(X, W, cols, S, FUN, tFUN, ...)
          }
)
