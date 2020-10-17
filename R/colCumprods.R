### ============================================================================
### colCumprods
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_colCumprods <- function(x, rows = NULL, cols = NULL,
                                             ...) {
  # Check input type
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = TRUE)

  # Subset
  x <- ..subset(x, rows, cols)

  # Compute result
  val <- colblock_APPLY(x = x,
                        FUN = colCumprods,
                        ...)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  # NOTE: Return value of matrixStats::colCumprods() has no names
  unname(do.call(cbind, val))
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit MatrixGenerics::colCumprods
#' @importMethodsFrom DelayedArray seed
#' @rdname colCummaxs
#' @export
#' @author Peter Hickey
#' @examples
#'
#' colCumprods(dm_matrix)
setMethod("colCumprods", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL,
                   force_block_processing = FALSE, ...) {
            .smart_seed_dispatcher(x, generic = "colCumprods", 
                                   blockfun = .DelayedMatrix_block_colCumprods,
                                   force_block_processing = force_block_processing,
                                   rows = rows,
                                   cols = cols,
                                   ...)
          }
)
