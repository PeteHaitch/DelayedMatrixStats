### =============================================================================
### rowMeans2
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_rowMeans2 <- function(x, rows = NULL, cols = NULL,
                                           na.rm = FALSE, ..., useNames = TRUE) {
  # Check input
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = TRUE)

  # Subset
  x <- ..subset(x, rows, cols)

  # Compute result
  val <- rowblock_APPLY(x = x,
                        FUN = rowMeans2,
                        na.rm = na.rm,
                        ...,
                        useNames = useNames)
  if (length(val) == 0L) {
    return(numeric(nrow(x)))
  }
  unlist(val, recursive = FALSE, use.names = useNames)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit MatrixGenerics::rowMeans2
#' @importMethodsFrom DelayedArray seed
#' @rdname colMeans2
#' @export
#' @examples
#'
#' # NOTE: Temporarily use verbose output to demonstrate which method is
#' #       which method is being used
#' options(DelayedMatrixStats.verbose = TRUE)
#' # By default, this uses a seed-aware method for a DelayedMatrix with a
#' # 'SolidRleArraySeed' seed
#' rowMeans2(dm_Rle)
#' # Alternatively, can use the block-processing strategy
#' rowMeans2(dm_Rle, force_block_processing = TRUE)
#' options(DelayedMatrixStats.verbose = FALSE)
setMethod("rowMeans2", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE,
                   force_block_processing = FALSE, ..., useNames = TRUE) {
            .smart_seed_dispatcher(x, generic = MatrixGenerics::rowMeans2,
                                   blockfun = .DelayedMatrix_block_rowMeans2,
                                   force_block_processing = force_block_processing,
                                   rows = rows,
                                   cols = cols,
                                   na.rm = na.rm,
                                   ...,
                                   useNames = useNames)
          }
)

# ------------------------------------------------------------------------------
# Seed-aware methods
#

#' @importMethodsFrom Matrix rowMeans
#' @rdname colMeans2
#' @export
setMethod("rowMeans2", "Matrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE,
                   ..., useNames = TRUE) {
            message2(class(x), get_verbose())
            x <- ..subset(x, rows, cols)
            # NOTE: Return value of matrixStats::rowMeans2() has no names
            # TODO: Obey top-level `useNames` argument.
            val <- rowMeans(x = x, na.rm = na.rm)
            if (!useNames) {
              val <- unname(val)
            }
            val
          }
)
