### ============================================================================
### colSds
###

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit MatrixGenerics::colSds
#' @importMethodsFrom DelayedArray seed
#' @rdname colMads
#' @export
#' @author Peter Hickey
#' @examples
#'
#' colSds(dm_df)
setMethod("colSds", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, center = NULL,
                   force_block_processing = FALSE,
                   ..., useNames = TRUE) {
            sqrt(colVars(x = x,
                         rows = rows,
                         cols = cols,
                         na.rm = na.rm,
                         center = center,
                         force_block_processing = force_block_processing,
                         ...,
                         useNames = useNames))
          }
)
