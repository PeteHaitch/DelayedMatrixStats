### ============================================================================
### colDiffs
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_colDiffs <- function(x, rows = NULL, cols = NULL, lag = 1L,
                                          differences = 1L, dim. = dim(x),
                                          ...) {
  # Check input type
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = TRUE)

  # Subset
  x <- ..subset(x, rows, cols)

  # Compute result
  val <- DelayedArray:::colblock_APPLY(x = x,
                                       APPLY = matrixStats::colDiffs,
                                       lag = lag,
                                       differences = differences,
                                       ...)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  # NOTE: Return value of matrixStats::colDiffs() has no names
  unname(do.call(cbind, val))
}


### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @importMethodsFrom DelayedArray seed
#' @rdname colDiffs
#' @template common_params
#' @template lowercase_x
#' @export
#' @template example_dm_matrix
#' @template example_dm_HDF5
#' @examples
#'
#' colDiffs(dm_matrix)
setMethod("colDiffs", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, lag = 1L, differences = 1L,
                   dim. = dim(x), force_block_processing = FALSE, ...) {
            if (!hasMethod("colDiffs", seedClass(x)) ||
                force_block_processing) {
              message2("Block processing", get_verbose())
              return(.DelayedMatrix_block_colDiffs(x = x,
                                                   rows = rows,
                                                   cols = cols,
                                                   lag = lag,
                                                   differences = differences,
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
                return(colDiffs(x = x,
                                rows = rows,
                                cols = cols,
                                lag = lag,
                                differences = differences,
                                dim. = dim.,
                                force_block_processing = TRUE,
                                ...))
              }
            }

            colDiffs(x = simple_seed_x,
                     rows = rows,
                     cols = cols,
                     lag = lag,
                     differences = differences,
                     dim. = dim.,
                     ...)
          }
)

# ------------------------------------------------------------------------------
# Seed-aware methods
#

#' @export
setMethod("colDiffs", "matrix", matrixStats::colDiffs)
