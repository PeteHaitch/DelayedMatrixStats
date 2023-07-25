### ============================================================================
### colWeightedSds
###

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit MatrixGenerics::colWeightedSds
#' @importMethodsFrom DelayedArray seed
#' @rdname colWeightedVars
#' @export
#' @template example_dm_Rle
#' @author Peter Hickey
#' @examples
#'
#' colWeightedSds(dm_Rle, w = 1 / rowMeans2(dm_Rle))
setMethod("colWeightedSds", "DelayedMatrix",
          function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE,
                   force_block_processing = FALSE, ..., useNames = TRUE) {
            sqrt(colWeightedVars(
              x = x,
              w = w,
              rows = rows,
              cols = cols,
              na.rm = na.rm,
              force_block_processing = force_block_processing,
              ...,
              useNames = useNames))
          }
)
