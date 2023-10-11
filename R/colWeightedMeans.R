### ============================================================================
### colWeightedMeans
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_colWeightedMeans <- function(x, w = NULL, rows = NULL,
                                                  cols = NULL, na.rm = FALSE,
                                                  ..., useNames = TRUE) {
  # Check input type
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = FALSE)

  # Check and subset 'w' (must be either NULL, or a numeric vector of
  # length 1 or 'nrow(x)')
  if (!is.null(w)) {
    stopifnot(is.numeric(w))
    if (length(w) != 1L) {
      stopifnot(length(w) == nrow(x))
      if (!is.null(rows))
        w <- w[rows]
    }
  }

  # Subset 'x'
  x <- ..subset(x, rows, cols)

  # Compute result
  val <- colblock_APPLY(x = x,
                        FUN = colWeightedMeans,
                        w = w,
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

#' @inherit MatrixGenerics::colWeightedMeans
#' @importMethodsFrom DelayedArray seed
#' @rdname colWeightedMeans
#' @template common_params
#' @template lowercase_x
#' @export
#' @template example_dm_MatrixMatrix
#' @author Peter Hickey
#' @examples
#'
#' colWeightedMeans(dm_Matrix)
#' # Specifying weights inversely proportional to rowwise variances
#' colWeightedMeans(dm_Matrix, w = 1 / rowVars(dm_Matrix))
setMethod("colWeightedMeans", "DelayedMatrix",
          function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE,
                   force_block_processing = FALSE, ..., useNames = TRUE) {
            .smart_seed_dispatcher(x, generic = MatrixGenerics::colWeightedMeans,
                                   blockfun = .DelayedMatrix_block_colWeightedMeans,
                                   force_block_processing = force_block_processing,
                                   w = w,
                                   rows = rows,
                                   cols = cols,
                                   na.rm = na.rm,
                                   ...,
                                   useNames = useNames)
          }
)
