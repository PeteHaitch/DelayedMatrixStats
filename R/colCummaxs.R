### ============================================================================
### colCummaxs
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_colCummaxs <- function(x, rows = NULL, cols = NULL,
                                            ..., useNames = NA) {
  # Check input type
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = TRUE)

  # Subset
  x <- ..subset(x, rows, cols)

  # Compute result
  val <- colblock_APPLY(x = x,
                        FUN = colCummaxs,
                        ...,
                        useNames = useNames)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  # NOTE: Return value of matrixStats::colCummaxs() has no names
  # TODO: Obey top-level `useNames` argument.
  unname(do.call(cbind, val))
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit MatrixGenerics::colCummaxs
#' @importMethodsFrom DelayedArray seed
#' @rdname colCummaxs
#' @template common_params
#' @template lowercase_x
#' @template useNamesParameter
#' @export
#' @template example_dm_matrix
#' @template example_dm_MatrixMatrix
#' @author Peter Hickey
#' @examples
#'
#' colCummaxs(dm_matrix)
setMethod("colCummaxs", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL,
                   force_block_processing = FALSE, ..., useNames = NA) {
            .smart_seed_dispatcher(x, generic = MatrixGenerics::colCummaxs,
                                   blockfun = .DelayedMatrix_block_colCummaxs,
                                   force_block_processing = force_block_processing,
                                   rows = rows,
                                   cols = cols,
                                   ...,
                                   useNames = useNames)
          }
)
