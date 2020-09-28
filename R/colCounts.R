### ============================================================================
### colCounts
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_colCounts <- function(x, rows = NULL, cols = NULL,
                                           value = TRUE, na.rm = FALSE,
                                           ...) {
  # Check input type
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = FALSE)

  # Subset
  x <- ..subset(x, rows, cols)

  # Compute result
  val <- DelayedArray:::colblock_APPLY(x = x,
                                       APPLY = matrixStats::colCounts,
                                       value = value,
                                       na.rm = na.rm,
                                       ...)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  # NOTE: Return value of matrixStats::colCounts() has no names
  unlist(val, recursive = FALSE, use.names = FALSE)
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
                   force_block_processing = FALSE, ...) {
            .smart_seed_dispatcher(x, generic = "colCounts", 
                                   blockfun = .DelayedMatrix_block_colCounts,
                                   force_block_processing = force_block_processing,
                                   rows = rows,
                                   cols = cols,
                                   value = value,
                                   na.rm = na.rm,
                                   ...)
          }
)
