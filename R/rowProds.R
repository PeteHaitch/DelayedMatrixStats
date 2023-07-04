### =============================================================================
### rowProds
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_rowProds <- function(x, rows = NULL, cols = NULL,
                                          na.rm = FALSE,
                                          method = c("direct", "expSumLog"),
                                          ..., useNames = TRUE) {
  # Check input
  method <- match.arg(method)
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = FALSE)

  # Subset
  x <- ..subset(x, rows, cols)

  # Compute result
  val <- rowblock_APPLY(x = x,
                        FUN = rowProds,
                        na.rm = na.rm,
                        method = method,
                        ...,
                        useNames = useNames)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  unlist(val, recursive = FALSE, use.names = useNames)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit MatrixGenerics::rowProds
#' @importMethodsFrom DelayedArray seed
#' @rdname colProds
#' @export
#' @examples
#'
#' rowProds(dm_matrix)
setMethod("rowProds", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE,
                   method = c("direct", "expSumLog"),
                   force_block_processing = FALSE, ..., useNames = TRUE) {
            method <- match.arg(method)
            .smart_seed_dispatcher(x, generic = MatrixGenerics::rowProds,
                                   blockfun = .DelayedMatrix_block_rowProds,
                                   force_block_processing = force_block_processing,
                                   rows = rows,
                                   cols = cols,
                                   na.rm = na.rm,
#                                   method = method, # Wait for fix on SMS's side.
                                   ...,
                                   useNames = useNames)
          }
)
