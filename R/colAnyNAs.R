### ============================================================================
### colAnyNAs
###

### ----------------------------------------------------------------------------
### Exported methods
###

.DelayedMatrix_block_colAnyNAs <- function(x, rows, cols, ...) {
    colAnys(x = x, rows = rows, cols = cols, value = NA, ...)
}

# ------------------------------------------------------------------------------
# General method
#

#' @inherit MatrixGenerics::colAnyNAs
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
            .smart_seed_dispatcher(x, generic = "colAnyNAs", 
                                   blockfun = .DelayedMatrix_block_colAnyNAs,
                                   force_block_processing = force_block_processing,
                                   rows = rows,
                                   cols = cols,
                                   ...)
          }
)
