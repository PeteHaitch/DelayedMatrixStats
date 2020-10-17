### ============================================================================
### colLogSumExps
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_colLogSumExps <- function(lx, rows = NULL, cols = NULL,
                                               na.rm = FALSE, 
                                               ...) {
  # Check input type
  stopifnot(is(lx, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(lx, must.be.numeric = TRUE)

  # Subset
  lx <- ..subset(lx, rows, cols)

  # Compute result
  val <- colblock_APPLY(x = lx,
                        FUN = colLogSumExps,
                        na.rm = na.rm,
                        ...)
  if (length(val) == 0L) {
    return(numeric(ncol(lx)))
  }
  # NOTE: Return value of matrixStats::colLogSumExps() has names
  unlist(val, recursive = FALSE, use.names = TRUE)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit MatrixGenerics::colLogSumExps
#' @importMethodsFrom DelayedArray seed
#' @rdname colLogSumExps
#' @template common_params
#' @template lx
#' @export
#' @author Peter Hickey
#' @examples
#' x <- DelayedArray(matrix(runif(10), ncol = 2))
#' colLogSumExps(log(x))
setMethod("colLogSumExps", "DelayedMatrix",
          function(lx, rows = NULL, cols = NULL, na.rm = FALSE, 
                   force_block_processing = FALSE, ...) {
            .smart_seed_dispatcher(lx, generic = "colLogSumExps", 
                                   blockfun = .DelayedMatrix_block_colLogSumExps,
                                   force_block_processing = force_block_processing,
                                   rows = rows,
                                   cols = cols,
                                   na.rm  = na.rm,
                                   ...)
          }
)
