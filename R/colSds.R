### ============================================================================
### colSds
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
#' colSds(dm_df)
setMethod("colSds", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, force_block_processing = FALSE,
                   ...) {
            sqrt(colVars(x = x,
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
setMethod("colSds", "matrix", matrixStats::colSds)
