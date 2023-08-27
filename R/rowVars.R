### ============================================================================
### rowVars
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_rowVars <- function(x, rows = NULL, cols = NULL,
                                         na.rm = FALSE, center = NULL,
                                         ..., useNames = NA) {
  # Check input type
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = TRUE)

  # Check and subset 'center' (must be either NULL, or a numeric vector of
  # length 1 or 'nrow(x)')
  if (!is.null(center)) {
    stopifnot(is.numeric(center))
    if (length(center) != 1L) {
      stopifnot(length(center) == nrow(x))
      if (!is.null(rows))
        center <- center[rows]
    }
  }

  # Subset 'x'
  x <- ..subset(x, rows, cols)

  # Compute result
  DelayedArray:::BLOCK_rowVars(
    x,
    na.rm = na.rm,
    center = center,
    useNames = isTRUE(useNames))
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit MatrixGenerics::rowVars
#' @importMethodsFrom DelayedArray seed
#' @rdname colVars
#' @export
#' @examples
#'
#' rowVars(dm_matrix)
setMethod("rowVars", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, center = NULL,
                   force_block_processing = FALSE, ..., useNames = NA) {
            .smart_seed_dispatcher(x, generic = MatrixGenerics::rowVars,
                                   blockfun = .DelayedMatrix_block_rowVars,
                                   force_block_processing = force_block_processing,
                                   rows = rows,
                                   cols = cols,
                                   na.rm = na.rm,
                                   center = center,
                                   ...,
                                   useNames = useNames)
          }
)
