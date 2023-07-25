### ============================================================================
### colCummins
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_colCummins <- function(x, rows = NULL, cols = NULL,
                                            ..., useNames = TRUE) {
  # Check input type
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = TRUE)

  # Subset
  x <- ..subset(x, rows, cols)

  # Compute result
  val <- colblock_APPLY(x = x,
                        FUN = colCummins,
                        ...,
                        useNames = useNames)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  val <- do.call(cbind, val)
  if (!useNames) {
    val <- unname(val)
  }
  val
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit MatrixGenerics::colCummins
#' @importMethodsFrom DelayedArray seed
#' @rdname colCummaxs
#' @export
#' @author Peter Hickey
#' @examples
#'
#' colCummins(dm_matrix)
setMethod("colCummins", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL,
                   force_block_processing = FALSE, ..., useNames = TRUE) {
            .smart_seed_dispatcher(x, generic = MatrixGenerics::colCummins,
                                   blockfun = .DelayedMatrix_block_colCummins,
                                   force_block_processing = force_block_processing,
                                   rows = rows,
                                   cols = cols,
                                   ...,
                                   useNames = useNames)
          }
)
