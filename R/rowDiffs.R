### ============================================================================
### rowDiffs
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_rowDiffs <- function(x, rows = NULL, cols = NULL, lag = 1L,
                                          differences = 1L,
                                          ..., useNames = TRUE) {
  # Check input type
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = TRUE)

  # Subset
  x <- ..subset(x, rows, cols)

  # Compute result
  val <- rowblock_APPLY(x = x,
                        FUN = rowDiffs,
                        lag = lag,
                        differences = differences,
                        ...,
                        useNames = useNames)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  val <- do.call(rbind, val)
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

#' @inherit MatrixGenerics::rowDiffs
#' @importMethodsFrom DelayedArray seed
#' @rdname colDiffs
#' @export
#' @examples
#'
#' rowDiffs(dm_HDF5)
#' # In reverse column order
#' rowDiffs(dm_HDF5, cols = seq(ncol(dm_HDF5), 1, -1))
setMethod("rowDiffs", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, lag = 1L, differences = 1L,
                   force_block_processing = FALSE, ..., useNames = TRUE) {
            .smart_seed_dispatcher(x, generic = MatrixGenerics::rowDiffs,
                                   blockfun = .DelayedMatrix_block_rowDiffs,
                                   force_block_processing = force_block_processing,
                                   rows = rows,
                                   cols = cols,
                                   lag = lag,
                                   differences = differences,
                                   ...,
                                   useNames = useNames)
          }
)
