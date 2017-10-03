### ============================================================================
### colLogSumExps
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

#' `colLogSumExps()` block-processing internal helper
#' @inherit matrixStats::colLogSumExps
#' @importFrom methods is
.DelayedMatrix_block_colLogSumExps <- function(lx, rows = NULL, cols = NULL,
                                               na.rm = FALSE, dim. = dim(lx),
                                               ...) {
  # Check input type
  stopifnot(is(lx, "DelayedMatrix"))
  stopifnot(!lx@is_transposed)
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

#' @importFrom DelayedArray seed
#' @importFrom methods hasMethod is
#' @rdname colLogSumExps
#' @template common_params
#' @template lx
#' @export
setMethod("colLogSumExps", "DelayedMatrix",
          function(lx, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(lx),
                   force_block_processing = FALSE, ...) {
            if (!hasMethod("colLogSumExps", class(seed(lx))) ||
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
            if (DelayedArray:::is_pristine(lx)) {
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

# ------------------------------------------------------------------------------
# Seed-aware methods
#

#' @importFrom methods setMethod
#' @export
setMethod("colLogSumExps", "matrix", matrixStats::colLogSumExps)
