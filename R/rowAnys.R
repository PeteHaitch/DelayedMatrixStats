### ============================================================================
### rowAnys
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_rowAnys <- function(x, rows = NULL, cols = NULL,
                                         value = TRUE, na.rm = FALSE,
                                         ...) {
  # Check input
  stopifnot(is(x, "DelayedMatrix"))
  # TODO: Answer is always logical, so this might not be appropriate
  DelayedArray:::.get_ans_type(x, must.be.numeric = TRUE)

  # Subset
  x <- ..subset(x, rows, cols)

  # Compute result
  val <- rowblock_APPLY(x = x,
                        FUN = rowAnys,
                        value = value,
                        na.rm = na.rm,
                        ...)
  if (length(val) == 0L) {
    return(logical(ncol(x)))
  }
  # NOTE: Return value of matrixStats::rowAnys() has no names
  unlist(val, recursive = FALSE, use.names = FALSE)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit MatrixGenerics::rowAnys
#' @importMethodsFrom DelayedArray seed
#' @rdname colAlls
#' @export
#' @examples
#' rowAnys(dm_Rle, value = 2)
setMethod("rowAnys", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE,
                   force_block_processing = FALSE, ...) {
            .smart_seed_dispatcher(x, generic = "rowAnys", 
                                   blockfun = .DelayedMatrix_block_rowAnys,
                                   force_block_processing = force_block_processing,
                                   rows = rows,
                                   cols = cols,
                                   value = value,
                                   na.rm = na.rm,
                                   ...)
          }
)
