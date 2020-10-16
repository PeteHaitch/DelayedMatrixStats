### ============================================================================
### rowSdDiffs
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_rowSdDiffs <- function(x, rows = NULL, cols = NULL,
                                            na.rm = FALSE, diff = 1L,
                                            trim = 0, ...) {
  # Check input type
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = TRUE)

  # Subset
  x <- ..subset(x, rows, cols)

  # Compute result
  val <- rowblock_APPLY(x = x,
                        FUN = matrixStats::rowSdDiffs,
                        na.rm = na.rm,
                        diff = diff,
                        trim = trim,
                        ...)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  # NOTE: Return value of matrixStats::rowSdDiffs() has names
  unlist(val, recursive = FALSE, use.names = TRUE)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit MatrixGenerics::rowSdDiffs
#' @importMethodsFrom DelayedArray seed
#' @rdname colIQRDiffs
#' @export
#' @examples
#'
#' # Only using rows 2-4
#' rowSdDiffs(dm_Rle, rows = 2:4)
setMethod("rowSdDiffs", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L,
                   trim = 0, force_block_processing = FALSE, ...) {
            .smart_seed_dispatcher(x, generic = "rowSdDiffs", 
                                   blockfun = .DelayedMatrix_block_rowSdDiffs,
                                   force_block_processing = force_block_processing,
                                   rows = rows,
                                   cols = cols,
                                   na.rm = na.rm,
                                   diff = diff,
                                   trim = trim,
                                   ...)
          }
)
