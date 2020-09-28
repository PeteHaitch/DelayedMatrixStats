### ============================================================================
### rowLogSumExps
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_rowLogSumExps <- function(lx, rows = NULL, cols = NULL,
                                               na.rm = FALSE, 
                                               ...) {
  # Check input type
  stopifnot(is(lx, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(lx, must.be.numeric = TRUE)

  # Subset
  lx <- ..subset(lx, rows, cols)

  # Compute result
  val <- rowblock_APPLY(x = lx,
                        APPLY = matrixStats::rowLogSumExps,
                        na.rm = na.rm,
                        ...)
  if (length(val) == 0L) {
    return(numeric(ncol(lx)))
  }
  # NOTE: Return value of matrixStats::rowLogSumExps() has names
  unlist(val, recursive = FALSE, use.names = TRUE)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit MatrixGenerics::rowLogSumExps
#' @importMethodsFrom DelayedArray seed
#' @rdname colLogSumExps
#' @export
#'
#' @examples
#' rowLogSumExps(log(x))
setMethod("rowLogSumExps", "DelayedMatrix",
          function(lx, rows = NULL, cols = NULL, na.rm = FALSE, 
                   force_block_processing = FALSE, ...) {
            .smart_seed_dispatcher(lx, generic = "rowLogSumExps", 
                                   blockfun = .DelayedMatrix_block_rowLogSumExps,
                                   force_block_processing = force_block_processing,
                                   rows = rows,
                                   cols = cols,
                                   na.rm  = na.rm,
                                   ...)
          }
)
