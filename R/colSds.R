### ============================================================================
### colSds
###

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit matrixStats::colSds
#' @importMethodsFrom DelayedArray seed
#' @rdname colMads
#' @export
#' @author Peter Hickey
#' @examples
#'
#' colSds(dm_df)
setMethod("colSds", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, center = NULL,
                   dim. = dim(x), force_block_processing = FALSE,
                   ...) {
            sqrt(colVars(x = x,
                         rows = rows,
                         cols = cols,
                         na.rm = na.rm,
                         center = center,
                         dim. = dim.,
                         force_block_processing = force_block_processing,
                         ...))
          }
)
