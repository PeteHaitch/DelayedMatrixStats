### ============================================================================
### rowCumprods
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_rowCumprods <- function(x, rows = NULL, cols = NULL,
                                             ...) {
  # Check input type
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = TRUE)

  # Subset
  x <- ..subset(x, rows, cols)

  # Compute result
  val <- rowblock_APPLY(x = x,
                        FUN = matrixStats::rowCumprods,
                        ...)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  # NOTE: Return value of matrixStats::rowCumprods() has no names
  unname(do.call(rbind, val))
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit MatrixGenerics::rowCumprods
#' @importMethodsFrom DelayedArray seed
#' @rdname colCummaxs
#' @export
#' @examples
#'
#' # Only use rows 2-4
#' rowCumprods(dm_Matrix, rows = 2:4)
setMethod("rowCumprods", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL,
                   force_block_processing = FALSE, ...) {
            .smart_seed_dispatcher(x, generic = "rowCumprods", 
                                   blockfun = .DelayedMatrix_block_rowCumprods,
                                   force_block_processing = force_block_processing,
                                   rows = rows,
                                   cols = cols,
                                   ...)
          }
)
