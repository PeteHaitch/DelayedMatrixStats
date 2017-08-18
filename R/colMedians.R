### ============================================================================
### colMedians
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

#' `colMedians()` block-processing internal helper
#' @inherit matrixStats::colMedians
#' @importFrom methods is
.DelayedMatrix_block_colMedians <- function(x, rows = NULL, cols = NULL,
                                            na.rm = FALSE, dim. = dim(x), ...) {
  # Check input type
  stopifnot(is(x, "DelayedMatrix"))
  stopifnot(!x@is_transposed)
  DelayedArray:::.get_ans_type(x)

  # Subset
  x <- ..subset(x, rows = rows, cols = cols)

  # Compute result
  val <- DelayedArray:::colblock_APPLY(x,
                                       matrixStats::colMedians,
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

#' @importFrom DelayedArray seed
#' @importFrom methods hasMethod is
#' @rdname colMedians
#' @template common_params
#' @export
setMethod("colMedians", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                   force_block_processing = FALSE, ...) {
            if (!hasMethod("colMedians", class(seed(x))) ||
                force_block_processing) {
              message2("Block processing", get_verbose())
              return(.DelayedMatrix_block_colMedians(x, rows, cols, na.rm,
                                                     dim., ...))
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
                return(colMedians(x, rows, cols, na.rm, dim.,
                                  force_block_processing = TRUE, ...))
              }
            }

            colMedians(simple_seed_x, rows, cols, na.rm, dim., ...)
          }
)

# ------------------------------------------------------------------------------
# Seed-aware methods
#

#' @importFrom methods setMethod
#' @export
setMethod("colMedians", "matrix", matrixStats::colMedians)
