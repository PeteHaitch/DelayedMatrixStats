### ============================================================================
### colLogSumExps
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_colLogSumExps <- function(lx, rows = NULL, cols = NULL,
                                               na.rm = FALSE,
                                               ..., useNames = TRUE) {
  # Check input type
  stopifnot(is(lx, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(lx, must.be.numeric = TRUE)

  # Subset
  lx <- ..subset(lx, rows, cols)

  # Compute result
  val <- colblock_APPLY(x = lx,
                        FUN = colLogSumExps,
                        na.rm = na.rm,
                        ...,
                        useNames = useNames)
  if (length(val) == 0L) {
    return(numeric(ncol(lx)))
  }
  unlist(val, recursive = FALSE, use.names = useNames)
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
                   force_block_processing = FALSE, ..., useNames = TRUE) {
            .smart_seed_dispatcher(lx, generic = MatrixGenerics::colLogSumExps,
                                   blockfun = .DelayedMatrix_block_colLogSumExps,
                                   force_block_processing = force_block_processing,
                                   rows = rows,
                                   cols = cols,
                                   na.rm  = na.rm,
                                   ...,
                                   useNames = useNames)
          }
)
