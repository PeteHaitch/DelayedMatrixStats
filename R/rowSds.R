### ============================================================================
### rowSds
###

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit MatrixGenerics::rowSds
#' @importMethodsFrom DelayedArray seed
#' @rdname colMads
#' @export
#' @examples
#'
#' rowSds(dm_DF)
setMethod("rowSds", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, center = NULL,
                   dim. = dim(x), force_block_processing = FALSE, ...) {
            sqrt(rowVars(x = x,
                         rows = rows,
                         cols = cols,
                         na.rm = na.rm,
                         center = center,
                         dim. = dim.,
                         force_block_processing = force_block_processing,
                         ...))
          }
)
