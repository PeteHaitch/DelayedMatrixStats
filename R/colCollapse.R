### ============================================================================
### colCollapse
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_colCollapse <- function(x, idxs, cols = NULL,
                                             ..., useNames = TRUE) {
  # Check input
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = FALSE)

  # Subset
  x <- ..subset(x, cols = cols)
  if (!is.null(cols)) {
    idxs <- idxs[cols]
  }

  # Compute result
  IDXS <- rep_len(idxs, ncol(x))
  val <- colblock_APPLY(x = x,
                        FUN = .colCollapse_internal,
                        idxs = IDXS,
                        ...,
                        useNames = useNames)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  unlist(val, recursive = FALSE, use.names = useNames)
}

#' @importFrom DelayedArray currentViewport makeNindexFromArrayViewport
.colCollapse_internal <- function(x, idxs, ..., useNames = TRUE) {
    block.env <- parent.frame(2)
    vp <- currentViewport(block.env)
    subset <- makeNindexFromArrayViewport(vp)[[2]]
    if (!is.null(subset)) {
        idxs <- idxs[as.integer(subset)]
    }
    matrixStats::colCollapse(x, idxs, ..., useNames = useNames)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit MatrixGenerics::colCollapse
#' @importMethodsFrom DelayedArray seed
#' @rdname colCollapse
#' @template common_params
#' @template lowercase_x
#' @export
#' @template example_dm_matrix
#' @template example_dm_HDF5
#' @author Peter Hickey
#' @examples
#'
#' # Extract the 4th row as a vector
#' # NOTE: An ordinary vector is returned regardless of the backend of
#' #       the DelayedMatrix object
#' colCollapse(dm_matrix, 4)
#' colCollapse(dm_HDF5, 4)
setMethod("colCollapse", "DelayedMatrix",
          function(x, idxs, cols = NULL,
                   force_block_processing = FALSE, ..., useNames = TRUE) {
            .smart_seed_dispatcher(x, generic = MatrixGenerics::colCollapse,
                                   blockfun = .DelayedMatrix_block_colCollapse,
                                   force_block_processing = force_block_processing,
                                   idxs = idxs,
                                   cols = cols,
                                   ...,
                                   useNames = useNames)
          }
)
