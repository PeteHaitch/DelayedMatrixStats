### ============================================================================
### rowMads
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_rowMads <- function(x, rows = NULL, cols = NULL,
                                         center = NULL, constant = 1.4826,
                                         na.rm = FALSE, ...) {
  # Check input type
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = TRUE)

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
                        FUN = .rowMads_internal,
                        center = center,
                        constant = constant,
                        na.rm = na.rm,
                        ...)
  if (length(val) == 0L) {
    return(numeric(nrow(x)))
  }
  # NOTE: Return value of matrixStats::rowMads() has no names
  unlist(val, recursive = FALSE, use.names = FALSE)
}

#' @importFrom DelayedArray currentViewport makeNindexFromArrayViewport
.rowMads_internal <- function(x, center, ...) {
    if (!is.null(center) && length(center) != 1L) {
        block.env <- parent.frame(2)
        vp <- currentViewport(block.env)
        subset <- makeNindexFromArrayViewport(vp)[[1]]
        if (!is.null(subset)) {
            center <- center[as.integer(subset)]
        }
    }
    rowMads(x, center = center, ...)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit MatrixGenerics::rowMads
#' @importMethodsFrom DelayedArray seed
#' @rdname colMads
#' @export
#' @examples
#'
#' rowMads(dm_DF)
setMethod("rowMads", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, center = NULL,
                   constant = 1.4826, na.rm = FALSE, 
                   force_block_processing = FALSE, ...) {
            .smart_seed_dispatcher(x, generic = MatrixGenerics::rowMads, 
                                   blockfun = .DelayedMatrix_block_rowMads,
                                   force_block_processing = force_block_processing,
                                   rows = rows,
                                   cols = cols,
                                   center = center,
                                   constant = constant,
                                   na.rm = na.rm,
                                   ...)
          }
)
