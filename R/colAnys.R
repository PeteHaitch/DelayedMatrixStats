### ============================================================================
### colAnys
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_colAnys <- function(x, rows = NULL, cols = NULL,
                                         value = TRUE, na.rm = FALSE,
                                         ..., useNames = TRUE) {
  # Check input
  stopifnot(is(x, "DelayedMatrix"))
  # TODO: Answer is always logical, so this might not be appropriate
  DelayedArray:::.get_ans_type(x, must.be.numeric = FALSE)

  # Subset
  x <- ..subset(x, rows, cols)

  # Compute result
  val <- colblock_APPLY(x = x,
                        FUN = colAnys,
                        value = value,
                        na.rm = na.rm,
                        ...,
                        useNames = useNames)
  if (length(val) == 0L) {
    return(logical(ncol(x)))
  }
  unlist(val, recursive = FALSE, use.names = useNames)
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
                   force_block_processing = FALSE, ..., useNames = TRUE) {
            .smart_seed_dispatcher(x, generic = MatrixGenerics::colAnys,
                                   blockfun = .DelayedMatrix_block_colAnys,
                                   force_block_processing = force_block_processing,
                                   rows = rows,
                                   cols = cols,
                                   value = value,
                                   na.rm = na.rm,
                                   ...,
                                   useNames = useNames)
          }
)
