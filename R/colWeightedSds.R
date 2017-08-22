### ============================================================================
### colWeightedSds
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

#' `colWeightedSds()` block-processing internal helper
#' @inherit matrixStats::colWeightedSds
#' @importFrom methods is
.DelayedMatrix_block_colWeightedSds <- function(x, w = NULL, rows = NULL,
                                                cols = NULL, na.rm = FALSE,
                                                ...) {
  # Check input type
  stopifnot(is(x, "DelayedMatrix"))
  stopifnot(!x@is_transposed)
  DelayedArray:::.get_ans_type(x)

  # Subset
  x <- ..subset(x, rows = rows, cols = cols)
  w <- w[DelayedArray:::to_linear_index(list(rows, NULL), c(length(w), 1L))]


  # Compute result
  val <- DelayedArray:::colblock_APPLY(x,
                                       matrixStats::colWeightedSds,
                                       w = w,
                                       na.rm = na.rm,
                                       ...)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  # NOTE: Return value of matrixStats::colWeightedSds() has names
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
#' @rdname colWeightedSds
#' @template common_params
#' @export
setMethod("colWeightedSds", "DelayedMatrix",
          function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE,
                   force_block_processing = FALSE, ...) {
            if (!hasMethod("colWeightedSds", class(seed(x))) ||
                force_block_processing) {
              message2("Block processing", get_verbose())
              return(.DelayedMatrix_block_colWeightedSds(x, w, rows, cols,
                                                         na.rm, ...))
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
                return(colWeightedSds(x, w, rows, cols, na.rm,
                                      force_block_processing = TRUE, ...))
              }
            }

            colWeightedSds(simple_seed_x, w, rows, cols, na.rm, ...)
          }
)

# ------------------------------------------------------------------------------
# Seed-aware methods
#

#' @importFrom methods setMethod
#' @export
setMethod("colWeightedSds", "matrix", matrixStats::colWeightedSds)
