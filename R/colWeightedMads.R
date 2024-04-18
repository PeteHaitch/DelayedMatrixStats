### ============================================================================
### colWeightedMads
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_colWeightedMads <- function(x, w = NULL, rows = NULL,
                                                 cols = NULL, na.rm = FALSE,
                                                 constant = 1.4826,
                                                 center = NULL, ..., useNames = TRUE) {
  # Check input type
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = FALSE)

  # Check and subset 'w' (must be either NULL, or a numeric vector of
  # length 1 or 'nrow(x)')
  if (!is.null(w)) {
    stopifnot(is.numeric(w))
    if (length(w) != 1L) {
      stopifnot(length(w) == nrow(x))
      if (!is.null(rows))
        w <- w[rows]
    }
  }

  # Check, normalize, and subset 'center'
  center <- normarg_center_and_subset(center, ncol(x), "ncol(x)", cols)

  # Subset 'x'
  x <- ..subset(x, rows, cols)

  # Compute result
  val <- colblock_APPLY(x = x,
                        FUN = .colWeightedMads_internal,
                        w = w,
                        na.rm = na.rm,
                        constant = constant,
                        center = center,
                        ...,
                        useNames = useNames)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  unlist(val, recursive = FALSE, use.names = useNames)
}

#' @importFrom DelayedArray currentViewport makeNindexFromArrayViewport
.colWeightedMads_internal <- function(x, center, ..., useNames = TRUE) {
    if (!is.null(center) && length(center) != 1L) {
        block.env <- parent.frame(2)
        vp <- currentViewport(block.env)
        subset <- makeNindexFromArrayViewport(vp)[[2]]
        if (!is.null(subset)) {
            center <- center[as.integer(subset)]
        }
    }
    colWeightedMads(x, center = center, ..., useNames = useNames)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit MatrixGenerics::colWeightedMads
#' @importMethodsFrom DelayedArray seed
#' @rdname colWeightedMads
#' @template common_params
#' @template lowercase_x
#' @export
#' @template example_dm_matrix
#' @author Peter Hickey
#' @examples
#'
#' colWeightedMads(dm_matrix, w = 1:5)
setMethod("colWeightedMads", "DelayedMatrix",
          function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE,
                   constant = 1.4826, center = NULL,
                   force_block_processing = FALSE, ..., useNames = TRUE) {
            .smart_seed_dispatcher(x, generic = MatrixGenerics::colWeightedMads,
                                   blockfun = .DelayedMatrix_block_colWeightedMads,
                                   force_block_processing = force_block_processing,
                                   w = w,
                                   rows = rows,
                                   cols = cols,
                                   na.rm = na.rm,
                                   constant = constant,
                                   center = center,
                                   ...,
                                   useNames = useNames)
          }
)
