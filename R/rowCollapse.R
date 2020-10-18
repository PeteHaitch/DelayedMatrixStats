### ============================================================================
### rowCollapse
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

#' @importMethodsFrom DelayedArray t
.DelayedMatrix_block_rowCollapse <- function(x, idxs, rows = NULL,
                                             ...) {
  # Check input
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = FALSE)

  # Subset
  x <- ..subset(x, rows = rows)
  if (!is.null(rows)) {
    idxs <- idxs[rows]
  }

  # Compute result
  IDXS <- rep_len(idxs, nrow(x))
  val <- rowblock_APPLY(x = x,
                        FUN = .rowCollapse_internal,
                        idxs = IDXS,
                        ...)
  if (length(val) == 0L) {
    return(numeric(nrow(x)))
  }
  # NOTE: Return value of matrixStats::rowCollapse() has no names
  unlist(val, recursive = TRUE, use.names = FALSE)
}

#' @importFrom DelayedArray currentViewport makeNindexFromArrayViewport
.rowCollapse_internal <- function(x, idxs, ...) {
    block.env <- parent.frame(2)
    vp <- currentViewport(block.env)
    subset <- makeNindexFromArrayViewport(vp)[[1]]
    if (!is.null(subset)) {
        idxs <- idxs[subset]
    }
    matrixStats::rowCollapse(x, idxs, ...)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit MatrixGenerics::rowCollapse
#' @importMethodsFrom DelayedArray seed
#' @rdname colCollapse
#' @export
#' @examples
#'
#' # Extract the 2nd column as a vector
#' # NOTE: An ordinary vector is returned regardless of the backend of
#' #       the DelayedMatrix object
#' rowCollapse(dm_matrix, 2)
#' rowCollapse(dm_HDF5, 2)
setMethod("rowCollapse", "DelayedMatrix",
          function(x, idxs, rows = NULL, 
                   force_block_processing = FALSE, ...) {
            .smart_seed_dispatcher(x, generic = MatrixGenerics::rowCollapse, 
                                   blockfun = .DelayedMatrix_block_rowCollapse,
                                   force_block_processing = force_block_processing,
                                   idxs = idxs,
                                   rows = rows,
                                   ...)
          }
)
