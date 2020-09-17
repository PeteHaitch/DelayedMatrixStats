### ============================================================================
### colLogSumExps
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_colLogSumExps <- function(lx, rows = NULL, cols = NULL,
                                               na.rm = FALSE, dim. = dim(lx),
                                               ...) {
  # Check input type
  stopifnot(is(lx, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(lx, must.be.numeric = TRUE)

  # Subset
  lx <- ..subset(lx, rows, cols)

  # Compute result
  val <- DelayedArray:::colblock_APPLY(x = lx,
                                       APPLY = matrixStats::colLogSumExps,
                                       na.rm = na.rm,
                                       ...)
  if (length(val) == 0L) {
    return(numeric(ncol(lx)))
  }
  # NOTE: Return value of matrixStats::colLogSumExps() has names
  unlist(val, recursive = FALSE, use.names = TRUE)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit matrixStats::colLogSumExps
#' @importMethodsFrom DelayedArray seed
#' @rdname colLogSumExps
#' @template common_params
#' @template lx
#' @export
#' @author Peter Hickey
#' @examples
#' x <- DelayedArray(matrix(runif(10), ncol = 2))
#' colLogSumExps(log(x))
setMethod("colLogSumExps", "DelayedMatrix",
          function(lx, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(lx),
                   force_block_processing = FALSE, ...) {
            if (!hasMethod("colLogSumExps", seedClass(lx)) ||
                force_block_processing) {
              message2("Block processing", get_verbose())
              return(.DelayedMatrix_block_colLogSumExps(lx = lx,
                                                        rows = rows,
                                                        cols = cols,
                                                        na.rm  = na.rm,
                                                        dim. = dim.,
                                                        ...))
            }

            message2("Has seed-aware method", get_verbose())
            if (isPristine(lx)) {
              message2("Pristine", get_verbose())
              simple_seed_lx <- seed(lx)
            } else {
              message2("Coercing to seed class", get_verbose())
              # TODO: do_transpose trick
              simple_seed_lx <- try(from_DelayedArray_to_simple_seed_class(lx),
                                    silent = TRUE)
              if (is(simple_seed_lx, "try-error")) {
                message2("Unable to coerce to seed class", get_verbose())
                return(colLogSumExps(lx = lx,
                                     rows = rows,
                                     cols = cols,
                                     na.rm = na.rm,
                                     dim. = dim.,
                                     force_block_processing = TRUE,
                                     ...))
              }
            }

            colLogSumExps(lx = simple_seed_lx,
                          rows = rows,
                          cols = cols,
                          na.rm = na.rm,
                          dim. = dim.,
                          ...)
          }
)
