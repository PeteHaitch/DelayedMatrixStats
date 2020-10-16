### ============================================================================
### colAnys
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_colAnys <- function(x, rows = NULL, cols = NULL,
                                         value = TRUE, na.rm = FALSE,
                                         ...) {
  # Check input
  stopifnot(is(x, "DelayedMatrix"))
  # TODO: Answer is always logical, so this might not be appropriate
  DelayedArray:::.get_ans_type(x, must.be.numeric = FALSE)

  # Subset
  x <- ..subset(x, rows, cols)

  # Compute result
  val <- colblock_APPLY(x = x,
                        FUN = matrixStats::colAnys,
                        value = value,
                        na.rm = na.rm,
                        ...)
  if (length(val) == 0L) {
    return(logical(ncol(x)))
  }
  # NOTE: Return value of matrixStats::colAnys() has no names
  unlist(val, recursive = FALSE, use.names = FALSE)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit MatrixGenerics::colAnys
#' @importMethodsFrom DelayedArray seed
#' @rdname colAlls
#' @export
#' @author Peter Hickey
#' @examples
#' colAnys(dm_matrix, value = 2)
setMethod("colAnys", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE,
                   force_block_processing = FALSE, ...) {
            .smart_seed_dispatcher(x, generic = "colAnys", 
                                   blockfun = .DelayedMatrix_block_colAnys,
                                   force_block_processing = force_block_processing,
                                   rows = rows,
                                   cols = cols,
                                   value = value,
                                   na.rm = na.rm,
                                   ...)
          }
)
