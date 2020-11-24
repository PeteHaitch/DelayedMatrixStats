### ============================================================================
### rowWeightedMads
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_rowWeightedMads <- function(x, w = NULL, rows = NULL,
                                                 cols = NULL, na.rm = FALSE,
                                                 constant = 1.4826,
                                                 center = NULL, ...) {
  # Check input type
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = FALSE)

  # Check and subset 'w' (must be either NULL, or a numeric vector of
  # length 1 or 'ncol(x)')
  if (!is.null(w)) {
    stopifnot(is.numeric(w))
    if (length(w) != 1L) {
      stopifnot(length(w) == ncol(x))
      if (!is.null(cols))
        w <- w[cols]
    }
  }

  # Check and subset 'center' (must be either NULL, or a numeric vector of
  # length 1 or 'nrow(x)')
  if (!is.null(center)) {
    stopifnot(is.numeric(center))
    if (length(center) != 1L) {
      stopifnot(length(center) == nrow(x))
      if (!is.null(rows))
        center <- center[rows]
    }
  }

  # Subset 'x'
  x <- ..subset(x, rows, cols)

  # Compute result
  val <- rowblock_APPLY(x = x,
                        FUN = .rowWeightedMads_internal,
                        w = w,
                        na.rm = na.rm,
                        constant = constant,
                        center = center,
                        ...)
  if (length(val) == 0L) {
    return(numeric(nrow(x)))
  }
  # NOTE: Return value of matrixStats::rowWeightedMads() has names
  unlist(val, recursive = FALSE, use.names = TRUE)
}

#' @importFrom DelayedArray currentViewport makeNindexFromArrayViewport
.rowWeightedMads_internal <- function(x, center, ...) {
    if (!is.null(center) && length(center) != 1L) {
        block.env <- parent.frame(2)
        vp <- currentViewport(block.env)
        subset <- makeNindexFromArrayViewport(vp)[[1]]
        if (!is.null(subset)) {
            center <- center[as.integer(subset)]
        }
    }
    rowWeightedMads(x, center = center, ...)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit MatrixGenerics::rowWeightedMads
#' @importMethodsFrom DelayedArray seed
#' @rdname colWeightedMads
#' @export
#' @examples
#'
#' rowWeightedMads(dm_matrix, w = 3:1)
setMethod("rowWeightedMads", "DelayedMatrix",
          function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE,
                   constant = 1.4826, center = NULL,
                   force_block_processing = FALSE, ...) {
            .smart_seed_dispatcher(x, generic = MatrixGenerics::rowWeightedMads, 
                                   blockfun = .DelayedMatrix_block_rowWeightedMads,
                                   force_block_processing = force_block_processing,
                                   w = w,
                                   rows = rows,
                                   cols = cols,
                                   na.rm = na.rm,
                                   constant = constant,
                                   center = center,
                                   ...)
          }
)

