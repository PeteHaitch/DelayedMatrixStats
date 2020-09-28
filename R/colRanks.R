### ============================================================================
### colRanks
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_colRanks <-
  function(x, rows = NULL, cols = NULL,
           ties.method = c("max", "average", "first", "last", "random", "max", "min", "dense"),
           preserveShape = FALSE, ...) {
    # Check input type
    ties.method <- match.arg(ties.method)
    stopifnot(is(x, "DelayedMatrix"))
    DelayedArray:::.get_ans_type(x, must.be.numeric = TRUE)

    # Subset
    x <- ..subset(x, rows, cols)

    # Compute result
    val <- DelayedArray:::colblock_APPLY(x = x,
                                         APPLY = matrixStats::colRanks,
                                         ties.method = ties.method,
                                         preserveShape = preserveShape,
                                         ...)
    if (length(val) == 0L) {
      return(numeric(ncol(x)))
    }
    # NOTE: Return value of matrixStats::colRanks() has no names
    unname(do.call(rbind, val))
  }

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit MatrixGenerics::colRanks
#' @importMethodsFrom DelayedArray seed
#' @rdname colRanks
#' @template common_params
#' @template lowercase_x
#' @export
#' @template example_dm_MatrixMatrix
#' @author Peter Hickey
#' @examples
#'
#' colRanks(dm_Matrix)
setMethod("colRanks", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL,
                   ties.method = c("max", "average", "first", "last", "random", "max", "min", "dense"),
                   preserveShape = FALSE,
                   force_block_processing = FALSE, ...) {
            ties.method <- match.arg(ties.method)
            .smart_seed_dispatcher(x, generic = "colRanks", 
                                   blockfun = .DelayedMatrix_block_colRanks,
                                   force_block_processing = force_block_processing,
                                   rows = rows,
                                   cols = cols,
                                   ties.method = ties.method,
                                   # preserveShape = preserveShape, # TODO: wait for a fix in SMS.
                                   ...)
          }
)
