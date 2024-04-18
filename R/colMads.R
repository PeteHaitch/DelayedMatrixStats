### ============================================================================
### colMads
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_colMads <- function(x, rows = NULL, cols = NULL,
                                         center = NULL, constant = 1.4826,
                                         na.rm = FALSE, ..., useNames = TRUE) {
  # Check input type
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = TRUE)

  # Check, normalize, and subset 'center'
  center <- normarg_center_and_subset(center, ncol(x), "ncol(x)", cols)

  # Subset 'x'
  x <- ..subset(x, rows, cols)

  # Compute result
  val <- colblock_APPLY(x = x,
                        FUN = .colMads_internal,
                        center = center,
                        constant = constant,
                        na.rm = na.rm,
                        ...,
                        useNames = useNames)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  unlist(val, recursive = FALSE, use.names = useNames)
}

#' @importFrom DelayedArray currentViewport makeNindexFromArrayViewport
.colMads_internal <- function(x, center, ..., useNames = TRUE) {
    if (!is.null(center) && length(center) != 1L) {
        block.env <- parent.frame(2)
        vp <- currentViewport(block.env)
        subset <- makeNindexFromArrayViewport(vp)[[2]]
        if (!is.null(subset)) {
            center <- center[as.integer(subset)]
        }
    }
    colMads(x, center = center, ..., useNames = useNames)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit MatrixGenerics::colMads
#' @importMethodsFrom DelayedArray seed
#' @rdname colMads
#' @template common_params
#' @template lowercase_x
#' @export
#' @template example_dm_df
#' @template example_dm_S4VectorsDF
#' @author Peter Hickey
#' @examples
#'
#' colMads(dm_df)
setMethod("colMads", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, center = NULL,
                   constant = 1.4826, na.rm = FALSE,
                   force_block_processing = FALSE, ..., useNames = TRUE) {
            .smart_seed_dispatcher(x, generic = MatrixGenerics::colMads,
                                   blockfun = .DelayedMatrix_block_colMads,
                                   force_block_processing = force_block_processing,
                                   rows = rows,
                                   cols = cols,
                                   center = center,
                                   constant = constant,
                                   na.rm = na.rm,
                                   ...,
                                   useNames = useNames)
          }
)
