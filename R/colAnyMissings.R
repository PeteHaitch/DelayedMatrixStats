### ============================================================================
### colAnyMissings
###

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit matrixStats::colAnyNAs
#' @importMethodsFrom DelayedArray seed
#' @rdname colAnyNAs
#' @author Peter Hickey
#' @export
setMethod("colAnyMissings", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, force_block_processing = FALSE,
                   ...) {
            colAnys(x = x,
                    rows = rows,
                    cols = cols,
                    value = NA,
                    force_block_processing = force_block_processing,
                    ...)
          }
)

# ------------------------------------------------------------------------------
# Seed-aware methods
#

#' @export
setMethod("colAnyMissings", "matrix", matrixStats::colAnyMissings)
