### ============================================================================
### rowIQRs
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_rowIQRs <- function(x, rows = NULL, cols = NULL,
                                         na.rm = FALSE, ...) {
  # Check input type
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = FALSE)

  # Subset
  x <- ..subset(x, rows, cols)

  # Compute result
  val <- rowblock_APPLY(x = x,
                        FUN = rowIQRs,
                        na.rm = na.rm,
                        ...)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  # NOTE: Return value of matrixStats::rowIQRs() has no names
  unlist(val, recursive = FALSE, use.names = FALSE)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit MatrixGenerics::rowIQRs
#' @importMethodsFrom DelayedArray seed
#' @rdname colIQRs
#' @export
#' @examples
#'
#' # Only using rows 2-4
#' rowIQRs(dm_matrix, rows = 2:4)
setMethod("rowIQRs", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE,
                   force_block_processing = FALSE, ...) {
            .smart_seed_dispatcher(x, generic = "rowIQRs", 
                                   blockfun = .DelayedMatrix_block_rowIQRs,
                                   force_block_processing = force_block_processing,
                                   rows = rows,
                                   cols = cols,
                                   na.rm = na.rm,
                                   ...)
          }
)
