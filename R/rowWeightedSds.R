### ============================================================================
### rowWeightedSds
###

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit MatrixGenerics::rowWeightedSds
#' @importFrom MatrixGenerics rowWeightedSds
#' @importMethodsFrom DelayedArray seed
#' @rdname colWeightedVars
#' @export
#' @examples
#'
#' # Specifying weights inversely proportional to columnwise means
#' rowWeightedSds(dm_Rle, w = 1 / colMeans2(dm_Rle))
setMethod("rowWeightedSds", "DelayedMatrix",
          function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE,
                   force_block_processing = FALSE, ...) {
            sqrt(rowWeightedVars(
              x = x,
              w = w,
              rows = rows,
              cols = cols,
              na.rm = na.rm,
              force_block_processing = force_block_processing,
              ...))
          }
)
