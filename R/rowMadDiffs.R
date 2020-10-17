### ============================================================================
### rowMadDiffs
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_rowMadDiffs <- function(x, rows = NULL, cols = NULL,
                                              na.rm = FALSE, diff = 1L,
                                              trim = 0, ...) {
  # Check input type
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = TRUE)

  # Subset
  x <- ..subset(x, rows, cols)

  # Compute result
  val <- rowblock_APPLY(x = x,
                        FUN = rowMadDiffs,
                        na.rm = na.rm,
                        diff = diff,
                        trim = trim,
                        ...)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  # NOTE: Return value of matrixStats::colIQRDiffs() has names
  unlist(val, recursive = FALSE, use.names = TRUE)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit MatrixGenerics::rowMadDiffs
#' @importMethodsFrom DelayedArray seed
#' @rdname colIQRDiffs
#' @export
#' @examples
#'
#' # Only using rows 2-4
#' rowMadDiffs(dm_Rle, rows = 2:4)
setMethod("rowMadDiffs", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L,
                   trim = 0, force_block_processing = FALSE, ...) {
            .smart_seed_dispatcher(x, generic = "rowMadDiffs", 
                                   blockfun = .DelayedMatrix_block_rowMadDiffs,
                                   force_block_processing = force_block_processing,
                                   rows = rows,
                                   cols = cols,
                                   na.rm = na.rm,
                                   diff = diff,
                                   trim = trim,
                                   ...)
          }
)
