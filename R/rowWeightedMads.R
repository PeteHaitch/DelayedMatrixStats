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

  # Subset
  x <- ..subset(x, rows, cols)
  if (!is.null(w) && !is.null(cols)) {
    w <- w[cols]
  }

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
    if (!is.null(center)) {
        block.env <- parent.frame(2)
        vp <- currentViewport(block.env)
        subset <- makeNindexFromArrayViewport(vp)[[1]]
        if (!is.null(subset)) {
            center <- center[subset]
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
            .smart_seed_dispatcher(x, generic = "rowWeightedMads", 
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

