### ============================================================================
### colIQRDiffs
###

###-----------------------------------------------------------------------------
### Non-exported methods
###

#' `colIQRDiffs()` block-processing internal helper
#' @inherit matrixStats::colIQRDiffs
#' @importFrom methods is
.DelayedMatrix_block_colIQRDiffs <- function(x, rows = NULL, cols = NULL,
                                             na.rm = FALSE, diff = 1L,
                                             trim = 0, ...) {
  # Check input type
  stopifnot(is(x, "DelayedMatrix"))
  stopifnot(!x@is_transposed)
  DelayedArray:::.get_ans_type(x)

  # Subset
  x <- ..subset(x, rows = rows, cols = cols)

  # Compute result
  val <- DelayedArray:::colblock_APPLY(x,
                                       matrixStats::colIQRDiffs,
                                       na.rm = na.rm,
                                       diff = diff,
                                       trim = trim,
                                       ...)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  # NOTE: Return value of matrixStats::colIQRDiffs() has names
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
#' @rdname colIQRDiffs
#' @template common_params
#' @export
setMethod("colIQRDiffs", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L,
                   trim = 0, force_block_processing = FALSE, ...) {
            if (!hasMethod("colIQRDiffs", class(seed(x))) ||
                force_block_processing) {
              message2("Block processing", get_verbose())
              return(.DelayedMatrix_block_colIQRDiffs(x, rows, cols, na.rm,
                                                      diff, trim, ...))
            }

            message2("Has seed-aware method", get_verbose())
            if (DelayedArray:::is_pristine(x)) {
              message2("Pristine", get_verbose())
              simple_seed_x <- seed(x)
            } else {
              message2("Coercing to seed class", get_verbose())
              # TODO: do_transpose trick
              simple_seed_x <- try(from_DelayedArray_to_simple_seed_class(x),
                                   silent = TRUE)
              if (is(simple_seed_x, "try-error")) {
                message2("Unable to coerce to seed class", get_verbose())
                return(colIQRDiffs(x, rows, cols, na.rm, diff, trim,
                                   force_block_processing = TRUE, ...))
              }
            }

            colIQRDiffs(simple_seed_x, rows, cols, na.rm, diff, trim, ...)
          }
)

# ------------------------------------------------------------------------------
# Seed-aware methods
#

#' @importFrom methods setMethod
#' @export
setMethod("colIQRDiffs", "matrix", matrixStats::colIQRDiffs)
