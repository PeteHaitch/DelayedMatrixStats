### ============================================================================
### rowVars
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_rowVars <- function(x, rows = NULL, cols = NULL,
                                         na.rm = FALSE, center = NULL,
                                         ...) {
  # Check input type
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = TRUE)

  # Subset
  x <- ..subset(x, rows, cols)

  # Compute result
  val <- rowblock_APPLY(x = x,
                        FUN = .rowVars_internal,
                        na.rm = na.rm,
                        center = center,
                        ...)
  if (length(val) == 0L) {
    return(numeric(nrow(x)))
  }
  # NOTE: Return value of matrixStats::rowVars() has no names
  unlist(val, recursive = FALSE, use.names = FALSE)
}

#' @importFrom DelayedArray currentViewport makeNindexFromArrayViewport
.rowVars_internal <- function(x, center, ...) {
    if (!is.null(center)) {
        block.env <- parent.frame(2)
        vp <- currentViewport(block.env)
        subset <- makeNindexFromArrayViewport(vp)[[1]]
        if (!is.null(subset)) {
            center <- center[subset]
        }
    }
    rowVars(x, center = center, ...)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit MatrixGenerics::rowVars
#' @importMethodsFrom DelayedArray seed
#' @rdname colVars
#' @export
#' @examples
#'
#' rowVars(dm_matrix)
setMethod("rowVars", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, center = NULL,
                   force_block_processing = FALSE, ...) {
            .smart_seed_dispatcher(x, generic = MatrixGenerics::rowVars, 
                                   blockfun = .DelayedMatrix_block_rowVars,
                                   force_block_processing = force_block_processing,
                                   rows = rows,
                                   cols = cols,
                                   na.rm = na.rm,
                                   center = center,
                                   ...)
          }
)
