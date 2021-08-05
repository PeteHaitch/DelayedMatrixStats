### ============================================================================
### colVars
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_colVars <- function(x, rows = NULL, cols = NULL,
                                         na.rm = FALSE, center = NULL,
                                         ..., useNames = NA) {
  # Check input type
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = TRUE)

  # Check and subset 'center' (must be either NULL, or a numeric vector of
  # length 1 or 'ncol(x)')
  if (!is.null(center)) {
    stopifnot(is.numeric(center))
    if (length(center) != 1L) {
      stopifnot(length(center) == ncol(x))
      if (!is.null(cols))
        center <- center[cols]
    }
  }

  # Subset 'x'
  x <- ..subset(x, rows, cols)

  # Compute result
  val <- colblock_APPLY(x = x,
                        FUN = .colVars_internal,
                        na.rm = na.rm,
                        center = center,
                        ...,
                        useNames = useNames)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  # NOTE: Return value of matrixStats::colVars() has no names
  # TODO: Obey top-level `useNames` argument.
  unlist(val, recursive = FALSE, use.names = FALSE)
}

#' @importFrom DelayedArray currentViewport makeNindexFromArrayViewport
.colVars_internal <- function(x, center, ..., useNames = NA) {
    if (!is.null(center) && length(center) != 1L) {
        block.env <- parent.frame(2)
        vp <- currentViewport(block.env)
        subset <- makeNindexFromArrayViewport(vp)[[2]]
        if (!is.null(subset)) {
            center <- center[as.integer(subset)]
        }
    }
    colVars(x, center = center, ..., useNames = useNames)
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
#' @template useNamesParameter
#' @export
#' @template example_dm_matrix
#' @template example_dm_HDF5
#' @author Peter Hickey
#' @examples
#'
#' colVars(dm_matrix)
setMethod("colVars", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, center = NULL,
                   force_block_processing = FALSE, ..., useNames = NA) {
            .smart_seed_dispatcher(x, generic = MatrixGenerics::colVars,
                                   blockfun = .DelayedMatrix_block_colVars,
                                   force_block_processing = force_block_processing,
                                   rows = rows,
                                   cols = cols,
                                   na.rm = na.rm,
                                   center = center,
                                   ...,
                                   useNames = useNames)
          }
)
