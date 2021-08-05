### ============================================================================
### colIQRs
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_colIQRs <- function(x, rows = NULL, cols = NULL,
                                         na.rm = FALSE, ..., useNames = NA) {
  # Check input type
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = FALSE)

  # Subset
  x <- ..subset(x, rows, cols)

  # Compute result
  val <- colblock_APPLY(x = x,
                        FUN = colIQRs,
                        na.rm = na.rm,
                        ...,
                        useNames = useNames)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  # NOTE: Return value of matrixStats::colIQRs() has no names
  # TODO: Obey top-level `useNames` argument.
  unlist(val, recursive = FALSE, use.names = FALSE)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit MatrixGenerics::colIQRs
#' @importMethodsFrom DelayedArray seed
#' @rdname colIQRs
#' @template common_params
#' @template lowercase_x
#' @template useNamesParameter
#' @export
#' @template example_dm_matrix
#' @template example_dm_MatrixMatrix
#' @author Peter Hickey
#' @examples
#'
#' colIQRs(dm_matrix)
setMethod("colIQRs", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE,
                   force_block_processing = FALSE, ..., useNames = NA) {
            .smart_seed_dispatcher(x, generic = MatrixGenerics::colIQRs,
                                   blockfun = .DelayedMatrix_block_colIQRs,
                                   force_block_processing = force_block_processing,
                                   rows = rows,
                                   cols = cols,
                                   na.rm = na.rm,
                                   ...,
                                   useNames = useNames)
          }
)
