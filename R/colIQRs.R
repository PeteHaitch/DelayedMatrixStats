### ============================================================================
### colIQRs
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

#' `colIQRs()` block-processing internal helper
#' @inherit matrixStats::colIQRs
#' @importFrom methods is
.DelayedMatrix_block_colIQRs <- function(x, rows = NULL, cols = NULL,
                                         na.rm = FALSE, ...) {
  # Check input type
  stopifnot(is(x, "DelayedMatrix"))
  stopifnot(!x@is_transposed)
  DelayedArray:::.get_ans_type(x)

  # Subset
  x <- ..subset(x, rows, cols)

  # Compute result
  val <- DelayedArray:::colblock_APPLY(x = x,
                                       APPLY = matrixStats::colIQRs,
                                       na.rm = na.rm,
                                       ...)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  # NOTE: Return value of matrixStats::colIQRs() has no names
  unlist(val, recursive = FALSE, use.names = FALSE)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @importFrom DelayedArray seed
#' @importFrom methods hasMethod is
#' @rdname colIQRs
#' @template common_params
#' @export
setMethod("colIQRs", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE,
                   force_block_processing = FALSE, ...) {
            if (!hasMethod("colIQRs", class(seed(x))) ||
                force_block_processing) {
              message2("Block processing", get_verbose())
              return(.DelayedMatrix_block_colIQRs(x = x,
                                                  rows = rows,
                                                  cols = cols,
                                                  na.rm = na.rm,
                                                  ...))
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
                return(colIQRs(x = x,
                               rows = rows,
                               cols = cols,
                               na.rm = na.rm,
                               force_block_processing = TRUE,
                               ...))
              }
            }

            colIQRs(x = simple_seed_x,
                    rows = rows,
                    cols = cols,
                    na.rm = na.rm,
                    ...)
          }
)

# ------------------------------------------------------------------------------
# Seed-aware methods
#

#' @importFrom methods setMethod
#' @export
setMethod("colIQRs", "matrix", matrixStats::colIQRs)
