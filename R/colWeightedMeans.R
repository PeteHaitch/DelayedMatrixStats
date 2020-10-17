### ============================================================================
### colWeightedMeans
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_colWeightedMeans <- function(x, w = NULL, rows = NULL,
                                                  cols = NULL, na.rm = FALSE,
                                                  ...) {
  # Check input type
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = FALSE)

  # Subset
  x <- ..subset(x, rows, cols)
  if (!is.null(w) && !is.null(rows)) {
    w <- w[rows]
  }

  # Compute result
  val <- colblock_APPLY(x = x,
                        FUN = colWeightedMeans,
                        w = w,
                        na.rm = na.rm,
                        ...)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  # NOTE: Return value of matrixStats::colWeightedMeans() has names
  unlist(val, recursive = FALSE, use.names = TRUE)
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
                   force_block_processing = FALSE, ...) {
            .smart_seed_dispatcher(x, generic = "colWeightedMeans", 
                                   blockfun = .DelayedMatrix_block_colWeightedMeans,
                                   force_block_processing = force_block_processing,
                                   w = w,
                                   rows = rows,
                                   cols = cols,
                                   na.rm = na.rm,
                                   ...)
          }
)
