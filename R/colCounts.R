### ============================================================================
### colCounts
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_colCounts <- function(x, rows = NULL, cols = NULL,
                                           value = TRUE, na.rm = FALSE,
                                           ..., useNames = TRUE) {
  # Check input type
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = FALSE)

  # Subset
  x <- ..subset(x, rows, cols)

  # Compute result
  val <- colblock_APPLY(x = x,
                        FUN = colCounts,
                        value = value,
                        na.rm = na.rm,
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

#' @inherit MatrixGenerics::colCounts
#' @importMethodsFrom DelayedArray seed
#' @rdname colCounts
#' @template common_params
#' @template lowercase_x
#' @export
#' @template example_dm_matrix
#' @template example_dm_S4VectorsDF
#' @author Peter Hickey
#' @examples
#'
#' colCounts(dm_matrix, value = 1)
#' # Only count those in the first 4 rows
#' colCounts(dm_matrix, rows = 1:4, value = 1)
setMethod("colCounts", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE,
                   force_block_processing = FALSE, ..., useNames = TRUE) {
            .smart_seed_dispatcher(x, generic = MatrixGenerics::colCounts,
                                   blockfun = .DelayedMatrix_block_colCounts,
                                   force_block_processing = force_block_processing,
                                   rows = rows,
                                   cols = cols,
                                   value = value,
                                   na.rm = na.rm,
                                   ...,
                                   useNames = useNames)
          }
)
