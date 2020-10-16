### ============================================================================
### colCummaxs
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_colCummaxs <- function(x, rows = NULL, cols = NULL,
                                            ...) {
  # Check input type
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = TRUE)

  # Subset
  x <- ..subset(x, rows, cols)

  # Compute result
  val <- colblock_APPLY(x = x,
                        FUN = matrixStats::colCummaxs,
                        ...)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  # NOTE: Return value of matrixStats::colCummaxs() has no names
  unname(do.call(cbind, val))
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit MatrixGenerics::colCummaxs
#' @importMethodsFrom DelayedArray seed
#' @rdname colCummaxs
#' @template common_params
#' @template lowercase_x
#' @export
#' @template example_dm_matrix
#' @template example_dm_MatrixMatrix
#' @author Peter Hickey
#' @examples
#'
#' colCummaxs(dm_matrix)
setMethod("colCummaxs", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL,
                   force_block_processing = FALSE, ...) {
            .smart_seed_dispatcher(x, generic = "colCummaxs", 
                                   blockfun = .DelayedMatrix_block_colCummaxs,
                                   force_block_processing = force_block_processing,
                                   rows = rows,
                                   cols = cols,
                                   ...)
          }
)
