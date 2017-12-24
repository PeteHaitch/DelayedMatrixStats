### ============================================================================
### colCounts
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

#' `colCounts()` block-processing internal helper
#' @inherit matrixStats::colCounts
.DelayedMatrix_block_colCounts <- function(x, rows = NULL, cols = NULL,
                                           value = TRUE, na.rm = FALSE,
                                           dim. = dim(x), ...) {
  # Check input type
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = FALSE)

  # Subset
  x <- ..subset(x, rows, cols)

  # Compute result
  val <- DelayedArray:::colblock_APPLY(x = x,
                                       APPLY = matrixStats::colCounts,
                                       value = value,
                                       na.rm = na.rm,
                                       ...)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  # NOTE: Return value of matrixStats::colCounts() has no names
  unlist(val, recursive = FALSE, use.names = FALSE)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @importMethodsFrom DelayedArray seed
#' @rdname colCounts
#' @template common_params
#' @template lowercase_x
#' @export
#' @template example_dm_matrix
#' @template example_dm_S4VectorsDF
#' @examples
#'
#' colCounts(dm_matrix, value = 1)
#' # Only count those in the first 4 rows
#' colCounts(dm_matrix, rows = 1:4, value = 1)
setMethod("colCounts", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE,
                   dim. = dim(x), force_block_processing = FALSE, ...) {
            if (!hasMethod("colCounts", class(seed(x))) ||
                force_block_processing) {
              message2("Block processing", get_verbose())
              return(.DelayedMatrix_block_colCounts(x = x,
                                                    rows = rows,
                                                    cols = cols,
                                                    value = value,
                                                    na.rm = na.rm,
                                                    dim. = dim.,
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
                return(colCounts(x = x,
                                 rows = rows,
                                 cols = cols,
                                 value = value,
                                 na.rm = na.rm,
                                 dim. = dim.,
                                 force_block_processing = TRUE,
                                 ...))
              }
            }

            colCounts(x = simple_seed_x,
                      rows = rows,
                      cols = cols,
                      value = value,
                      na.rm = na.rm,
                      dim. = dim.,
                      ...)
          }
)

# ------------------------------------------------------------------------------
# Seed-aware methods
#

#' @export
setMethod("colCounts", "matrix", matrixStats::colCounts)
