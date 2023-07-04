### ============================================================================
### rowRanks
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_rowRanks <-
  function(x, rows = NULL, cols = NULL,
           ties.method = c("max", "average", "first", "last", "random", "max", "min", "dense"), ..., useNames = TRUE) {
    # Check input type
    ties.method <- match.arg(ties.method)
    stopifnot(is(x, "DelayedMatrix"))
    DelayedArray:::.get_ans_type(x, must.be.numeric = TRUE)

    # Subset
    x <- ..subset(x, rows, cols)

    # Compute result
    val <- rowblock_APPLY(x = x,
                          FUN = rowRanks,
                          ties.method = ties.method,
                          ...,
                          useNames = useNames)
    if (length(val) == 0L) {
      return(numeric(ncol(x)))
    }
    val <- do.call(rbind, val)
    if (!useNames) {
      val <- unname(val)
    }
    val
  }

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit MatrixGenerics::rowRanks
#' @importMethodsFrom DelayedArray seed
#' @rdname colRanks
#' @export
#' @examples
#'
#' rowRanks(dm_Matrix)
setMethod("rowRanks", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL,
                   ties.method = c("max", "average", "first", "last", "random", "max", "min", "dense"),
                   force_block_processing = FALSE, ..., useNames = TRUE) {
            ties.method <- match.arg(ties.method)
            .smart_seed_dispatcher(x, generic = MatrixGenerics::rowRanks,
                                   blockfun = .DelayedMatrix_block_rowRanks,
                                   force_block_processing = force_block_processing,
                                   rows = rows,
                                   cols = cols,
                                   ties.method = ties.method,
                                   ...,
                                   useNames = useNames)
          }
)
