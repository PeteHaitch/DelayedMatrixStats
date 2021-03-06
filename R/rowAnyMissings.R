### =============================================================================
### rowAnyMissings
###

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit matrixStats::rowAnyMissings
#' @importMethodsFrom DelayedArray seed
#' @rdname colAnyNAs
#' @export
setMethod("rowAnyMissings", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, force_block_processing = FALSE,
                   ...) {
            rowAnyNAs(x = x,
                    rows = rows,
                    cols = cols,
                    force_block_processing = force_block_processing,
                    ...)
          }
)

# ------------------------------------------------------------------------------
# Seed-aware methods
#

#' @export
setMethod("rowAnyMissings", "matrix", matrixStats::rowAnyMissings)
