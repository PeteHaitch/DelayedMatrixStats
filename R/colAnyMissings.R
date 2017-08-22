### =============================================================================
### colAnyMissings
###

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @importFrom DelayedArray seed
#' @importFrom methods hasMethod is
#' @rdname colAnyMissings
#' @template common_params
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

#' @importFrom methods setMethod
#' @export
setMethod("colAnyMissings", "matrix", matrixStats::colAnyMissings)
