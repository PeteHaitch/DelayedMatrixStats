### ============================================================================
### rowLogSumExps
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_rowLogSumExps <- function(lx, rows = NULL, cols = NULL,
                                               na.rm = FALSE, dim. = dim(lx),
                                               ...) {
  # Check input type
  stopifnot(is(lx, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(lx, must.be.numeric = TRUE)

  # Subset
  lx <- ..subset(lx, rows, cols)

  # Compute result
  val <- rowblock_APPLY(x = lx,
                        APPLY = matrixStats::rowLogSumExps,
                        na.rm = na.rm,
                        ...)
  if (length(val) == 0L) {
    return(numeric(ncol(lx)))
  }
  # NOTE: Return value of matrixStats::rowLogSumExps() has names
  unlist(val, recursive = FALSE, use.names = TRUE)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @importMethodsFrom DelayedArray seed
#' @rdname colLogSumExps
#' @export
#'
#' @examples
#' rowLogSumExps(log(x))
setMethod("rowLogSumExps", "DelayedMatrix",
          function(lx, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(lx),
                   force_block_processing = FALSE, ...) {
            if (!hasMethod("rowLogSumExps", seedClass(lx)) ||
                force_block_processing) {
              message2("Block processing", get_verbose())
              return(.DelayedMatrix_block_rowLogSumExps(lx = lx,
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
                return(rowLogSumExps(lx = lx,
                                     rows = rows,
                                     cols = cols,
                                     na.rm = na.rm,
                                     dim. = dim.,
                                     force_block_processing = TRUE,
                                     ...))
              }
            }

            rowLogSumExps(lx = simple_seed_lx,
                          rows = rows,
                          cols = cols,
                          na.rm = na.rm,
                          dim. = dim.,
                          ...)
          }
)

# ------------------------------------------------------------------------------
# Seed-aware methods
#

#' @export
setMethod("rowLogSumExps", "matrix", matrixStats::rowLogSumExps)
