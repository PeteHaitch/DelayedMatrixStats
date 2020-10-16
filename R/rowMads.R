### ============================================================================
### rowMads
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_rowMads <- function(x, rows = NULL, cols = NULL,
                                         center = NULL, constant = 1.4826,
                                         na.rm = FALSE, ...) {
  # Check input type
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = TRUE)

  # Subset
  x <- ..subset(x, rows, cols)

  # Compute result
  val <- rowblock_APPLY(x = x,
                        FUN = matrixStats::rowMads,
                        center = center,
                        constant = constant,
                        na.rm = na.rm,
                        ...)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  # NOTE: Return value of matrixStats::rowMads() has no names
  unlist(val, recursive = FALSE, use.names = FALSE)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit MatrixGenerics::rowMads
#' @importMethodsFrom DelayedArray seed
#' @rdname colMads
#' @export
#' @examples
#'
#' rowMads(dm_DF)
setMethod("rowMads", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, center = NULL,
                   constant = 1.4826, na.rm = FALSE, 
                   force_block_processing = FALSE, ...) {
            .smart_seed_dispatcher(x, generic = "rowMads", 
                                   blockfun = .DelayedMatrix_block_rowMads,
                                   force_block_processing = force_block_processing,
                                   rows = rows,
                                   cols = cols,
                                   center = center,
                                   constant = constant,
                                   na.rm = na.rm,
                                   ...)
          }
)
