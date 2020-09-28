### ============================================================================
### rowAlls
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_rowAlls <- function(x, rows = NULL, cols = NULL,
                                         value = TRUE, na.rm = FALSE,
                                         ...) {
  # Check input
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = FALSE)

  # Subset
  x <- ..subset(x, rows, cols)

  # Compute result
  val <- rowblock_APPLY(x = x,
                        APPLY = matrixStats::rowAlls,
                        value = value,
                        na.rm = na.rm,
                        ...)
  if (length(val) == 0L) {
    return(logical(ncol(x)))
  }
  # NOTE: Return value of matrixStats::rowAlls() has no names
  unlist(val, recursive = FALSE, use.names = FALSE)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit MatrixGenerics::rowAlls
#' @importMethodsFrom DelayedArray seed
#' @rdname colAlls
#' @export
#' @examples
#' rowAlls(dm_Rle, value = 1)
setMethod("rowAlls", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE,
                   force_block_processing = FALSE, ...) {
            .smart_seed_dispatcher(x, generic = "rowAlls", 
                                   blockfun = .DelayedMatrix_block_rowAlls,
                                   force_block_processing = force_block_processing,
                                   rows = rows,
                                   cols = cols,
                                   value = value,
                                   na.rm = na.rm,
                                   ...)
          }
)
