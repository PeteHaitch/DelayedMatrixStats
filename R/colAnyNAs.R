### =============================================================================
### colAnyNAs
###

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @importFrom DelayedArray seed
#' @importFrom methods hasMethod is
#' @rdname colAnyNAs
#' @template common_params
#' @export
setMethod("colAnyNAs", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, force_block_processing = FALSE,
                   ...) {
            colAnys(x, rows, cols, value = NA,
                    force_block_processing = force_block_processing, ...)
          }
)

# ------------------------------------------------------------------------------
# Seed-aware methods
#

#' @importFrom methods setMethod
#' @export
setMethod("colAnyNAs", "matrix", matrixStats::colAnyNAs)
