### ============================================================================
### rowTabulates
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_rowTabulates <- function(x, rows = NULL, cols = NULL,
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
        unlist(rowblock_APPLY(x = x,
                              FUN = function(x) {
                                unique(as.vector(x))
                              }),
               use.names = FALSE)),
      na.last = TRUE)
  }
  val <- rowblock_APPLY(x = x,
                        FUN = rowTabulates,
                        values = values,
                        ...)
  if (length(val) == 0L) {
    return(matrix(0L,0,0))
  }
  # NOTE: Return value of matrixStats::rowTabulates() has names
  do.call(rbind, val)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit MatrixGenerics::rowTabulates
#' @importFrom DelayedArray type
#' @importMethodsFrom DelayedArray seed
#' @rdname colTabulates
#' @export
#' @examples
#'
#' rowTabulates(dm_DF)
setMethod("rowTabulates", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, values = NULL,
                   force_block_processing = FALSE, ...) {
            if (!type(x) %in% c("integer", "logical", "raw")) {
              stop("Argument 'x' is not of type integer, logical, or raw",
                   " (type = ", type(x), ")")
            }
            .smart_seed_dispatcher(x, generic = "rowTabulates", 
                                   blockfun = .DelayedMatrix_block_rowTabulates,
                                   force_block_processing = force_block_processing,
                                   rows = rows,
                                   cols = cols,
                                   values = values,
                                   ...)
          }
)
