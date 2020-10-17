### ============================================================================
### rowOrderStats
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_rowOrderStats <- function(x, rows = NULL, cols = NULL,
                                               which, ...) {
  # Check input type
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = TRUE)

  # Subset
  x <- ..subset(x, rows, cols)

  # Compute result
  val <- rowblock_APPLY(x = x,
                        FUN = rowOrderStats,
                        which = which,
                        ...)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  # NOTE: Return value of matrixStats::rowOrderStats() has no names
  unlist(val, recursive = FALSE, use.names = FALSE)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit MatrixGenerics::rowOrderStats
#' @importMethodsFrom DelayedArray seed
#' @rdname colOrderStats
#' @export
#' @examples
#'
#' # Different algorithms, specified by `which`, may give different results
#' rowOrderStats(dm_Matrix, which = 1)
#' rowOrderStats(dm_Matrix, which = 2)
setMethod("rowOrderStats", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, which, 
                   force_block_processing = FALSE, ...) {
            .smart_seed_dispatcher(x, generic = "rowOrderStats", 
                                   blockfun = .DelayedMatrix_block_rowOrderStats,
                                   force_block_processing = force_block_processing,
                                   rows = rows,
                                   cols = cols,
                                   which = which,
                                   ...)
          }
)
