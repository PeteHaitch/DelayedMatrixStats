### =============================================================================
### colAnyNAs
###

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @importMethodsFrom DelayedArray seed
#' @rdname colAnyNAs
#' @template common_params
#' @template lowercase_x
#' @export
setMethod("colAnyNAs", "DelayedMatrix",
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
setMethod("colAnyNAs", "matrix", matrixStats::colAnyNAs)
