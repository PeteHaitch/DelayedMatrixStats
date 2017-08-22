### =============================================================================
### rowAnyNAs
###

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @importFrom DelayedArray seed
#' @importFrom methods hasMethod is
#' @rdname rowAnyNAs
#' @template common_params
#' @export
setMethod("rowAnyNAs", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, force_block_processing = FALSE,
                   ...) {
            rowAnys(x = x,
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
setMethod("rowAnyNAs", "matrix", matrixStats::rowAnyNAs)
