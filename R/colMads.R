### ============================================================================
### colMads
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_colMads <- function(x, rows = NULL, cols = NULL,
                                         center = NULL, constant = 1.4826,
                                         na.rm = FALSE, ...) {
  # Check input type
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = TRUE)

  # Subset
  x <- ..subset(x, rows, cols)

  # Compute result
  val <- colblock_APPLY(x = x,
                        FUN = colMads,
                        center = center,
                        constant = constant,
                        na.rm = na.rm,
                        ...)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  # NOTE: Return value of matrixStats::colMads() has no names
  unlist(val, recursive = FALSE, use.names = FALSE)
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
                   force_block_processing = FALSE, ...) {
            .smart_seed_dispatcher(x, generic = "colMads", 
                                   blockfun = .DelayedMatrix_block_colMads,
                                   force_block_processing = force_block_processing,
                                   rows = rows,
                                   cols = cols,
                                   center = center,
                                   constant = constant,
                                   na.rm = na.rm,
                                   ...)
          }
)
