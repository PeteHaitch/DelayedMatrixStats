### ============================================================================
### colRanks
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_colRanks <-
  function(x, rows = NULL, cols = NULL,
           ties.method = c("max", "average", "first", "last", "random", "max", "min", "dense"),
           preserveShape = FALSE, ..., useNames = TRUE) {
    # Check input type
    ties.method <- match.arg(ties.method)
    stopifnot(is(x, "DelayedMatrix"))
    DelayedArray:::.get_ans_type(x, must.be.numeric = TRUE)

    # Subset
    x <- ..subset(x, rows, cols)

    # Compute result
    val <- colblock_APPLY(x = x,
                          FUN = colRanks,
                          ties.method = ties.method,
                          preserveShape = preserveShape,
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

#' @inherit MatrixGenerics::colRanks
#' @importMethodsFrom DelayedArray seed
#' @rdname colRanks
#' @template common_params
#' @template lowercase_x
#' @export
#' @template example_dm_MatrixMatrix
#' @author Peter Hickey
#' @examples
#'
#' colRanks(dm_Matrix)
setMethod("colRanks", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL,
                   ties.method = c("max", "average", "first", "last", "random", "max", "min", "dense"),
                   preserveShape = FALSE,
                   force_block_processing = FALSE, ..., useNames = TRUE) {
            ties.method <- match.arg(ties.method)
            .smart_seed_dispatcher(x, generic = MatrixGenerics::colRanks,
                                   blockfun = .DelayedMatrix_block_colRanks,
                                   force_block_processing = force_block_processing,
                                   rows = rows,
                                   cols = cols,
                                   ties.method = ties.method,
                                   # preserveShape = preserveShape, # TODO: wait for a fix in SMS.
                                   ...,
                                   useNames = useNames)
          }
)
