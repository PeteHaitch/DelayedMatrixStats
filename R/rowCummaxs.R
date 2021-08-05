### ============================================================================
### rowCummaxs
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_rowCummaxs <- function(x, rows = NULL, cols = NULL,
                                            ..., useNames = NA) {
  # Check input type
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = TRUE)

  # Subset
  x <- ..subset(x, rows, cols)

  # Compute result
  val <- rowblock_APPLY(x = x,
                        FUN = rowCummaxs,
                        ...,
                        useNames = useNames)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  # NOTE: Return value of matrixStats::rowCummaxs() has no names
  # TODO: Obey top-level `useNames` argument.
  unname(do.call(rbind, val))
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit MatrixGenerics::rowCummaxs
#' @importMethodsFrom DelayedArray seed
#' @rdname colCummaxs
#' @export
#' @examples
#'
#' # Only use rows 2-4
#' rowCummaxs(dm_Matrix, rows = 2:4)
setMethod("rowCummaxs", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL,
                   force_block_processing = FALSE, ..., useNames = NA) {
            .smart_seed_dispatcher(x, generic = MatrixGenerics::rowCummaxs,
                                   blockfun = .DelayedMatrix_block_rowCummaxs,
                                   force_block_processing = force_block_processing,
                                   rows = rows,
                                   cols = cols,
                                   ...,
                                   useNames = useNames)
          }
)
