### ============================================================================
### rowMads
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_rowMads <- function(x, rows = NULL, cols = NULL,
                                         center = NULL, constant = 1.4826,
                                         na.rm = FALSE, ..., useNames = TRUE) {
  # Check input type
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = TRUE)

  # Check, normalize, and subset 'center'
  center <- normarg_center_and_subset(center, nrow(x), "nrow(x)", rows)

  # Subset 'x'
  x <- ..subset(x, rows, cols)

  # Compute result
  val <- rowblock_APPLY(x = x,
                        FUN = .rowMads_internal,
                        center = center,
                        constant = constant,
                        na.rm = na.rm,
                        ...,
                        useNames = useNames)
  if (length(val) == 0L) {
    return(numeric(nrow(x)))
  }
  unlist(val, recursive = FALSE, use.names = useNames)
}

#' @importFrom DelayedArray currentViewport makeNindexFromArrayViewport
.rowMads_internal <- function(x, center, ..., useNames = TRUE) {
    if (!is.null(center) && length(center) != 1L) {
        block.env <- parent.frame(2)
        vp <- currentViewport(block.env)
        subset <- makeNindexFromArrayViewport(vp)[[1]]
        if (!is.null(subset)) {
            center <- center[as.integer(subset)]
        }
    }
    rowMads(x, center = center, ..., useNames = useNames)
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
                   force_block_processing = FALSE, ..., useNames = TRUE) {
            .smart_seed_dispatcher(x, generic = MatrixGenerics::rowMads,
                                   blockfun = .DelayedMatrix_block_rowMads,
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
