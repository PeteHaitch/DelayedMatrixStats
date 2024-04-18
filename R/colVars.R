### ============================================================================
### colVars
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_colVars <- function(x, rows = NULL, cols = NULL,
                                         na.rm = FALSE, center = NULL,
                                         ..., useNames = TRUE) {
  # Check input type
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = TRUE)

  # Check, normalize, and subset 'center'
  center <- normarg_center_and_subset(center, ncol(x), "ncol(x)", cols)

  # Subset 'x'
  x <- ..subset(x, rows, cols)

  # Compute result
  DelayedArray:::BLOCK_colVars(
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

#' @inherit MatrixGenerics::colVars
#' @importMethodsFrom DelayedArray seed
#' @rdname colVars
#' @template common_params
#' @template lowercase_x
#' @export
#' @template example_dm_matrix
#' @template example_dm_HDF5
#' @author Peter Hickey
#' @examples
#'
#' colVars(dm_matrix)
setMethod("colVars", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, center = NULL,
                   force_block_processing = FALSE, ..., useNames = TRUE) {
            .smart_seed_dispatcher(x, generic = MatrixGenerics::colVars,
                                   blockfun = .DelayedMatrix_block_colVars,
                                   force_block_processing = force_block_processing,
                                   rows = rows,
                                   cols = cols,
                                   na.rm = na.rm,
                                   center = center,
                                   ...,
                                   useNames = useNames)
          }
)
