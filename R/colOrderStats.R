### ============================================================================
### colOrderStats
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_colOrderStats <- function(x, rows = NULL, cols = NULL,
                                               which, ...) {
  # Check input type
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = TRUE)

  # Subset
  x <- ..subset(x, rows, cols)

  # Compute result
  val <- DelayedArray:::colblock_APPLY(x = x,
                                       APPLY = matrixStats::colOrderStats,
                                       which = which,
                                       ...)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  # NOTE: Return value of matrixStats::colOrderStats() has no names
  unlist(val, recursive = FALSE, use.names = FALSE)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit MatrixGenerics::colOrderStats
#' @importMethodsFrom DelayedArray seed
#' @rdname colOrderStats
#' @template common_params
#' @template lowercase_x
#' @export
#' @template example_dm_MatrixMatrix
#' @author Peter Hickey
#' @examples
#' # Only using columns 2-3
#' colOrderStats(dm_Matrix, cols = 2:3, which = 1)
setMethod("colOrderStats", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, which, 
                   force_block_processing = FALSE, ...) {
            .smart_seed_dispatcher(x, generic = "colOrderStats",
                                   blockfun = .DelayedMatrix_block_colOrderStats,
                                   force_block_processing = force_block_processing,
                                   rows = rows,
                                   cols = cols,
                                   which = which,
                                   ...)
          }
)
