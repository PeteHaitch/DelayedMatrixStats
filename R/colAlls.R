### ============================================================================
### colAlls
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_colAlls <- function(x, rows = NULL, cols = NULL,
                                         value = TRUE, na.rm = FALSE,
                                         ..., useNames = NA) {
  # Check input
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = FALSE)

  # Subset
  x <- ..subset(x, rows, cols)

  # Compute result
  val <- colblock_APPLY(x = x,
                        FUN = colAlls,
                        value = value,
                        na.rm = na.rm,
                        ...,
                        useNames = useNames)
  if (length(val) == 0L) {
    return(logical(ncol(x)))
  }
  # NOTE: Return value of matrixStats::colAlls() has no names
  # TODO: Obey top-level `useNames` argument.
  unlist(val, recursive = FALSE, use.names = FALSE)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit MatrixGenerics::colAlls
#' @importMethodsFrom DelayedArray seed
#' @rdname colAlls
#' @template common_params
#' @template lowercase_x
#' @export
#' @template example_dm_matrix
#' @template example_dm_Rle
#' @template useNamesParameter
#' @author Peter Hickey
#' @examples
#'
#' colAlls(dm_matrix, value = 1)
setMethod("colAlls", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE,
                   force_block_processing = FALSE, ..., useNames = NA) {
            .smart_seed_dispatcher(x, generic = MatrixGenerics::colAlls,
                                   blockfun = .DelayedMatrix_block_colAlls,
                                   force_block_processing = force_block_processing,
                                   rows = rows,
                                   cols = cols,
                                   value = value,
                                   na.rm = na.rm,
                                   ...,
                                   useNames = useNames)
          }
)
