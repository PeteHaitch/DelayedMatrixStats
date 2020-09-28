### ============================================================================
### colCumsums
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_colCumsums <- function(x, rows = NULL, cols = NULL,
                                            ...) {
  # Check input type
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = TRUE)

  # Subset
  x <- ..subset(x, rows, cols)

  # Compute result
  val <- DelayedArray:::colblock_APPLY(x = x,
                                       APPLY = matrixStats::colCumsums,
                                       ...)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  # NOTE: Return value of matrixStats::colCumsums() has no names
  unname(do.call(cbind, val))
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit MatrixGenerics::colCumsums
#' @importMethodsFrom DelayedArray seed
#' @rdname colCummaxs
#' @export
#' @author Peter Hickey
#' @examples
#'
#' colCumsums(dm_matrix)
setMethod("colCumsums", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL,
                   force_block_processing = FALSE, ...) {
            .smart_seed_dispatcher(x, generic = "colCumsums", 
                                   blockfun = .DelayedMatrix_block_colCumsums,
                                   force_block_processing = force_block_processing,
                                   rows = rows,
                                   cols = cols,
                                   ...)
          }
)
