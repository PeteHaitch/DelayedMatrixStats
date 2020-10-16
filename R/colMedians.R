### ============================================================================
### colMedians
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_colMedians <- function(x, rows = NULL, cols = NULL,
                                            na.rm = FALSE, ...) {
  # Check input type
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = TRUE)

  # Subset
  x <- ..subset(x, rows, cols)

  # Compute result
  val <- colblock_APPLY(x = x,
                        FUN = matrixStats::colMedians,
                        na.rm = na.rm,
                        ...)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  # NOTE: Return value of matrixStats::colMedians() has no names
  unlist(val, recursive = FALSE, use.names = FALSE)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit MatrixGenerics::colMedians
#' @importMethodsFrom DelayedArray seed
#' @rdname colMedians
#' @template common_params
#' @template lowercase_x
#' @export
#' @template example_dm_MatrixMatrix
#' @author Peter Hickey
#' @examples
#'
#' colMedians(dm_Matrix)
setMethod("colMedians", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, 
                   force_block_processing = FALSE, ...) {
            .smart_seed_dispatcher(x, generic = "colMedians", 
                                   blockfun = .DelayedMatrix_block_colMedians,
                                   force_block_processing = force_block_processing,
                                   rows = rows,
                                   cols = cols,
                                   na.rm = na.rm,
                                   ...)
          }
)
