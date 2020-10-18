### ============================================================================
### colVars
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_colVars <- function(x, rows = NULL, cols = NULL,
                                         na.rm = FALSE, center = NULL,
                                         ...) {
  # Check input type
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = TRUE)

  # Subset
  x <- ..subset(x, rows, cols)

  # Compute result
  val <- colblock_APPLY(x = x,
                        FUN = .colVars_internal,
                        na.rm = na.rm,
                        center = center,
                        ...)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  # NOTE: Return value of matrixStats::colVars() has no names
  unlist(val, recursive = FALSE, use.names = FALSE)
}

#' @importFrom DelayedArray currentViewport makeNindexFromArrayViewport
.colVars_internal <- function(x, center, ...) {
    if (!is.null(center)) {
        block.env <- parent.frame(2)
        vp <- currentViewport(block.env)
        subset <- makeNindexFromArrayViewport(vp)[[2]]
        if (!is.null(subset)) {
            center <- center[subset]
        }
    }
    colVars(x, center = center, ...)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit MatrixGenerics::colVars
#' @importMethodsFrom DelayedArray seed
#' @rdname colVars
#' @template common_params
#' @template lowercase_x
#' @export
#' @template example_dm_matrix
#' @template example_dm_HDF5
#' @author Peter Hickey
#' @examples
#'
#' colVars(dm_matrix)
setMethod("colVars", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, center = NULL,
                   force_block_processing = FALSE, ...) {
            .smart_seed_dispatcher(x, generic = MatrixGenerics::colVars, 
                                   blockfun = .DelayedMatrix_block_colVars,
                                   force_block_processing = force_block_processing,
                                   rows = rows,
                                   cols = cols,
                                   na.rm = na.rm,
                                   center = center,
                                   ...)
          }
)
