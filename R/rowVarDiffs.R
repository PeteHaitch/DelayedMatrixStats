### ============================================================================
### rowVarDiffs
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_rowVarDiffs <- function(x, rows = NULL, cols = NULL,
                                             na.rm = FALSE, diff = 1L,
                                             trim = 0, ..., useNames = NA) {
  # Check input type
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = TRUE)

  # Subset
  x <- ..subset(x, rows,  cols)

  # Compute result
  val <- rowblock_APPLY(x = x,
                        FUN = rowVarDiffs,
                        na.rm = na.rm,
                        diff = diff,
                        trim = trim,
                        ...,
                        useNames = useNames)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  # NOTE: Return value of matrixStats::rowVarDiffs() has names
  # TODO: Obey top-level `useNames` argument.
  unlist(val, recursive = FALSE, use.names = TRUE)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit MatrixGenerics::rowVarDiffs
#' @importMethodsFrom DelayedArray seed
#' @rdname colIQRDiffs
#' @export
#' @examples
#'
#' # Only using rows 2-4
#' rowVarDiffs(dm_Rle, rows = 2:4)
setMethod("rowVarDiffs", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L,
                   trim = 0, force_block_processing = FALSE, ..., useNames = NA) {
            .smart_seed_dispatcher(x, generic = MatrixGenerics::rowVarDiffs,
                                   blockfun = .DelayedMatrix_block_rowVarDiffs,
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
