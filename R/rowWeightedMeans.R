### ============================================================================
### rowWeightedMeans
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_rowWeightedMeans <- function(x, w = NULL, rows = NULL,
                                                  cols = NULL, na.rm = FALSE,
                                                  ...) {
  # Check input type
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = FALSE)

  # Subset
  x <- ..subset(x, rows, cols)
  if (!is.null(w) && !is.null(cols)) {
    w <- w[cols]
  }

  # Compute result
  val <- rowblock_APPLY(x = x,
                        FUN = rowWeightedMeans,
                        w = w,
                        na.rm = na.rm,
                        ...)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  # NOTE: Return value of matrixStats::rowWeightedMeans() has names
  unlist(val, recursive = FALSE, use.names = TRUE)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit MatrixGenerics::rowWeightedMeans
#' @importMethodsFrom DelayedArray seed
#' @rdname colWeightedMeans
#' @export
#' @examples
#' rowWeightedMeans(dm_Matrix, w = 1:3)
setMethod("rowWeightedMeans", "DelayedMatrix",
          function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE,
                   force_block_processing = FALSE, ...) {
              .smart_seed_dispatcher(x, generic="rowWeightedMeans",
                                     blockfun = .DelayedMatrix_block_rowWeightedMeans,
                                     force_block_processing = force_block_processing,
                                     w = w,
                                     rows = rows,
                                     cols = cols,
                                     na.rm = na.rm,
                                     ...)
          }
)
