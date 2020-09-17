### ============================================================================
### colAnyNAs
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
#' @template common_params
#' @template lowercase_x
#' @export
#' @template example_dm_matrix
#' @template example_dm_HDF5
#' @author Peter Hickey
#' @examples
#'
#' dm_matrix[dm_matrix > 3] <- NA
#' colAnyNAs(dm_matrix)
#' dm_HDF5[dm_HDF5 > 3] <- NA
#' rowAnyNAs(dm_HDF5)
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
