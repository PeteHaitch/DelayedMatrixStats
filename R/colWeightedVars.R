### ============================================================================
### colWeightedVars
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_colWeightedVars <- function(x, w = NULL, rows = NULL,
                                                    cols = NULL, na.rm = FALSE,
                                                    ..., useNames = TRUE) {
  # Check input type
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = TRUE)

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

  # Subset 'x'
  x <- ..subset(x, rows, cols)

  # Compute result
  val <- colblock_APPLY(x = x,
                        FUN = colWeightedVars,
                        w = w,
                        na.rm = na.rm,
                        ...,
                        useNames = useNames)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  unlist(val, recursive = FALSE, use.names = useNames)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit MatrixGenerics::colWeightedVars
#' @importMethodsFrom DelayedArray seed
#' @rdname colWeightedVars
#' @template common_params
#' @template lowercase_x
#' @template useNamesParameter
#' @export
#' @author Peter Hickey
#' @examples
#'
#' # Specifying weights inversely proportional to rowwise means
#' colWeightedVars(dm_Rle, w = 1 / rowMeans2(dm_Rle))
setMethod("colWeightedVars", "DelayedMatrix",
          function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE,
                   force_block_processing = FALSE, ..., useNames = TRUE) {
            .smart_seed_dispatcher(x, generic = MatrixGenerics::colWeightedVars,
                                   blockfun = .DelayedMatrix_block_colWeightedVars,
                                   force_block_processing = force_block_processing,
                                   w = w,
                                   rows = rows,
                                   cols = cols,
                                   na.rm = na.rm,
                                   ...,
                                   useNames = useNames)
          }
)
