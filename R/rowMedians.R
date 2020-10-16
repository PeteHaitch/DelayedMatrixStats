### ============================================================================
### rowMedians
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_rowMedians <- function(x, rows = NULL, cols = NULL,
                                            na.rm = FALSE, ...) {
  # Check input type
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = TRUE)

  # Subset
  x <- ..subset(x, rows, cols)

  # Compute result
  val <- rowblock_APPLY(x = x,
                        FUN = matrixStats::rowMedians,
                        na.rm = na.rm,
                        ...)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  # NOTE: Return value of matrixStats::rowMedians() has no names
  unlist(val, recursive = FALSE, use.names = FALSE)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit MatrixGenerics::rowMedians
#' @importMethodsFrom DelayedArray seed
#' @rdname colMedians
#' @export
#' @examples
#'
#' rowMedians(dm_Matrix)
setMethod("rowMedians", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, 
                   force_block_processing = FALSE, ...) {
            .smart_seed_dispatcher(x, generic = "rowMedians", 
                                   blockfun = .DelayedMatrix_block_rowMedians,
                                   force_block_processing = force_block_processing,
                                   rows = rows,
                                   cols = cols,
                                   na.rm = na.rm,
                                   ...)
          }
)
