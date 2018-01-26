### ============================================================================
### rowSds
###

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @importMethodsFrom DelayedArray seed
#' @rdname colMads
#' @export
#' @examples
#'
#' rowSds(dm_DF)
setMethod("rowSds", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, force_block_processing = FALSE,
                   ...) {
            sqrt(rowVars(x = x,
                         rows = rows,
                         cols = cols,
                         force_block_processing = force_block_processing,
                         ...))
          }
)

# ------------------------------------------------------------------------------
# Seed-aware methods
#

#' @export
setMethod("rowSds", "matrix", matrixStats::rowSds)
