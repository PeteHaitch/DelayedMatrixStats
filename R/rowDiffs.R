### ============================================================================
### rowDiffs
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_rowDiffs <- function(x, rows = NULL, cols = NULL, lag = 1L,
                                          differences = 1L, dim. = dim(x),
                                          ...) {
  # Check input type
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = TRUE)

  # Subset
  x <- ..subset(x, rows, cols)

  # Compute result
  val <- rowblock_APPLY(x = x,
                        APPLY = matrixStats::rowDiffs,
                        lag = lag,
                        differences = differences,
                        ...)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  # NOTE: Return value of matrixStats::rowDiffs() has no names
  unname(do.call(rbind, val))
}


### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit MatrixGenerics::rowDiffs
#' @importFrom MatrixGenerics rowDiffs
#' @importMethodsFrom DelayedArray seed
#' @rdname colDiffs
#' @export
#' @examples
#'
#' rowDiffs(dm_HDF5)
#' # In reverse column order
#' rowDiffs(dm_HDF5, cols = seq(ncol(dm_HDF5), 1, -1))
setMethod("rowDiffs", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, lag = 1L, differences = 1L,
                   dim. = dim(x), force_block_processing = FALSE, ...) {
            if (!hasMethod("rowDiffs", seedClass(x)) ||
                force_block_processing) {
              message2("Block processing", get_verbose())
              return(.DelayedMatrix_block_rowDiffs(x = x,
                                                   rows = rows,
                                                   cols = cols,
                                                   lag = lag,
                                                   differences = differences,
                                                   dim. = dim.,
                                                   ...))
            }

            message2("Has seed-aware method", get_verbose())
            if (isPristine(x)) {
              message2("Pristine", get_verbose())
              simple_seed_x <- seed(x)
            } else {
              message2("Coercing to seed class", get_verbose())
              # TODO: do_transpose trick
              simple_seed_x <- try(from_DelayedArray_to_simple_seed_class(x),
                                   silent = TRUE)
              if (is(simple_seed_x, "try-error")) {
                message2("Unable to coerce to seed class", get_verbose())
                return(rowDiffs(x = x,
                                rows = rows,
                                cols = cols,
                                lag = lag,
                                differences = differences,
                                dim. = dim.,
                                force_block_processing = TRUE,
                                ...))
              }
            }

            rowDiffs(x = simple_seed_x,
                     rows = rows,
                     cols = cols,
                     lag = lag,
                     differences = differences,
                     dim. = dim.,
                     ...)
          }
)
