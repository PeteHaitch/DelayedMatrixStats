### ============================================================================
### colTabulates
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_colTabulates <- function(x, rows = NULL, cols = NULL,
                                              values = NULL, ...) {
  # Check input type
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = FALSE)

  # Subset
  x <- ..subset(x, rows, cols)

  # Compute result
  if (is.null(values)) {
    # NOTE: Need to compute values from entire x (not block-by-block)
    values <- sort(
      unique(
        unlist(colblock_APPLY(x = x,
                              FUN = function(x) {
                                unique(as.vector(x))
                              }),
               use.names = FALSE)),
      na.last = TRUE)
  }
  val <- colblock_APPLY(x = x,
                        FUN = colTabulates,
                        values = values,
                        ...)
  if (length(val) == 0L) {
    return(matrix(0L,0,0))
  }
  # NOTE: Return value of matrixStats::colTabulates() has names
  do.call(rbind, val)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit MatrixGenerics::colTabulates
#' @importFrom DelayedArray type
#' @importMethodsFrom DelayedArray seed
#' @rdname colTabulates
#' @template common_params
#' @template lowercase_x
#' @export
#' @template example_dm_S4VectorsDF
#' @author Peter Hickey
#' @examples
#'
#' colTabulates(dm_DF)
setMethod("colTabulates", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, values = NULL,
                   force_block_processing = FALSE, ...) {
            if (!type(x) %in% c("integer", "logical", "raw")) {
              stop("Argument 'x' is not of type integer, logical, or raw",
                   " (type = ", type(x), ")")
            }
            .smart_seed_dispatcher(x, generic = MatrixGenerics::colTabulates, 
                                   blockfun = .DelayedMatrix_block_colTabulates,
                                   force_block_processing = force_block_processing,
                                   rows = rows,
                                   cols = cols,
                                   values = values,
                                   ...)
          }
)
