### ============================================================================
### colWeightedMads
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_colWeightedMads <- function(x, w = NULL, rows = NULL,
                                                 cols = NULL, na.rm = FALSE,
                                                 constant = 1.4826,
                                                 center = NULL, ...) {
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
                        FUN = matrixStats::colWeightedMads,
                        w = w,
                        na.rm = na.rm,
                        constant = constant,
                        center = center,
                        ...)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  # NOTE: Return value of matrixStats::colWeightedMads() has names
  unlist(val, recursive = FALSE, use.names = TRUE)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit MatrixGenerics::colWeightedMads
#' @importMethodsFrom DelayedArray seed
#' @rdname colWeightedMads
#' @template common_params
#' @template lowercase_x
#' @export
#' @template example_dm_matrix
#' @author Peter Hickey
#' @examples
#'
#' colWeightedMads(dm_matrix, w = 1:5)
setMethod("colWeightedMads", "DelayedMatrix",
          function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE,
                   constant = 1.4826, center = NULL,
                   force_block_processing = FALSE, ...) {
            .smart_seed_dispatcher(x, generic = "colWeightedMads", 
                                   blockfun = .DelayedMatrix_block_colWeightedMads,
                                   force_block_processing = force_block_processing,
                                   w = w,
                                   rows = rows,
                                   cols = cols,
                                   na.rm = na.rm,
                                   constant = constant,
                                   center = center,
                                   ...)
          }
)
