### ============================================================================
### colOrderStats
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_colOrderStats <- function(x, rows = NULL, cols = NULL,
                                               which, ..., useNames = TRUE) {
  # Check input type
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = TRUE)

  # Subset
  x <- ..subset(x, rows, cols)

  # Compute result
  val <- colblock_APPLY(x = x,
                        FUN = colOrderStats,
                        which = which,
                        ...,
                        useNames = useNames)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  unlist(val, recursive = FALSE, use.names = useNames)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit MatrixGenerics::colOrderStats
#' @importMethodsFrom DelayedArray seed
#' @rdname colOrderStats
#' @template common_params
#' @template lowercase_x
#' @template useNamesParameter
#' @export
#' @template example_dm_MatrixMatrix
#' @author Peter Hickey
#' @examples
#' # Only using columns 2-3
#' colOrderStats(dm_Matrix, cols = 2:3, which = 1)
setMethod("colOrderStats", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, which,
                   force_block_processing = FALSE, ..., useNames = TRUE) {
            .smart_seed_dispatcher(x, generic = MatrixGenerics::colOrderStats,
                                   blockfun = .DelayedMatrix_block_colOrderStats,
                                   force_block_processing = force_block_processing,
                                   rows = rows,
                                   cols = cols,
                                   which = which,
                                   ...,
                                   useNames = useNames)
          }
)
