### ============================================================================
### rowVars
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_rowVars <- function(x, rows = NULL, cols = NULL,
                                         na.rm = FALSE, center = NULL,
                                         ..., useNames = NA) {
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
                        FUN = .rowVars_internal,
                        na.rm = na.rm,
                        center = center,
                        ...,
                        useNames = useNames)
  if (length(val) == 0L) {
    return(numeric(nrow(x)))
  }
  # NOTE: Return value of matrixStats::rowVars() has no names
  # TODO: Obey top-level `useNames` argument.
  unlist(val, recursive = FALSE, use.names = FALSE)
}

#' @importFrom DelayedArray currentViewport makeNindexFromArrayViewport
.rowVars_internal <- function(x, center, ..., useNames = NA) {
    if (!is.null(center) && length(center) != 1L) {
        block.env <- parent.frame(2)
        vp <- currentViewport(block.env)
        subset <- makeNindexFromArrayViewport(vp)[[1]]
        if (!is.null(subset)) {
            center <- center[as.integer(subset)]
        }
    }
    rowVars(x, center = center, ..., useNames = useNames)
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
                   force_block_processing = FALSE, ..., useNames = NA) {
            .smart_seed_dispatcher(x, generic = MatrixGenerics::rowVars,
                                   blockfun = .DelayedMatrix_block_rowVars,
                                   force_block_processing = force_block_processing,
                                   rows = rows,
                                   cols = cols,
                                   na.rm = na.rm,
                                   center = center,
                                   ...,
                                   useNames = useNames)
          }
)
