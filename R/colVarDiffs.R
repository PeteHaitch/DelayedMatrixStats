### ============================================================================
### colVarDiffs
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_colVarDiffs <- function(x, rows = NULL, cols = NULL,
                                             na.rm = FALSE, diff = 1L,
                                             trim = 0, ..., useNames = NA) {
  # Check input type
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = TRUE)

  # Subset
  x <- ..subset(x, rows,  cols)

  # Compute result
  val <- colblock_APPLY(x = x,
                        FUN = colVarDiffs,
                        na.rm = na.rm,
                        diff = diff,
                        trim = trim,
                        ...,
                        useNames = useNames)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  # NOTE: Return value of matrixStats::colVarDiffs() has names
  # TODO: Obey top-level `useNames` argument.
  unlist(val, recursive = FALSE, use.names = TRUE)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit MatrixGenerics::colVarDiffs
#' @importMethodsFrom DelayedArray seed
#' @rdname colIQRDiffs
#' @export
#' @author Peter Hickey
#' @examples
#'
#' colVarDiffs(dm_Matrix)
setMethod("colVarDiffs", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L,
                   trim = 0, force_block_processing = FALSE, ..., useNames = NA) {
            .smart_seed_dispatcher(x, generic = MatrixGenerics::colVarDiffs,
                                   blockfun = .DelayedMatrix_block_colVarDiffs,
                                   force_block_processing = force_block_processing,
                                   rows = rows,
                                   cols = cols,
                                   na.rm = na.rm,
                                   diff = diff,
                                   trim = trim,
                                   ...,
                                   useNames = useNames)
          }
)
