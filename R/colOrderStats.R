### ============================================================================
### colOrderStats
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_colOrderStats <- function(x, rows = NULL, cols = NULL,
                                               which, dim. = dim(x), ...) {
  # Check input type
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = TRUE)

  # Subset
  x <- ..subset(x, rows, cols)

  # Compute result
  val <- DelayedArray:::colblock_APPLY(x = x,
                                       APPLY = matrixStats::colOrderStats,
                                       which = which,
                                       ...)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  # NOTE: Return value of matrixStats::colOrderStats() has no names
  unlist(val, recursive = FALSE, use.names = FALSE)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit matrixStats::colOrderStats
#' @importMethodsFrom DelayedArray seed
#' @rdname colOrderStats
#' @template common_params
#' @template lowercase_x
#' @export
#' @template example_dm_MatrixMatrix
#' @author Peter Hickey
#' @examples
#' # Only using columns 2-3
#' colOrderStats(dm_Matrix, cols = 2:3, which = 1)
setMethod("colOrderStats", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, which, dim. = dim(x),
                   force_block_processing = FALSE, ...) {
            if (!hasMethod("colOrderStats", seedClass(x)) ||
                force_block_processing) {
              message2("Block processing", get_verbose())
              return(.DelayedMatrix_block_colOrderStats(x = x,
                                                        rows = rows,
                                                        cols = cols,
                                                        which = which,
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
                return(colOrderStats(x = x,
                                     rows = rows,
                                     cols = cols,
                                     which = which,
                                     dim. = dim.,
                                     force_block_processing = TRUE,
                                     ...))
              }
            }

            colOrderStats(x = simple_seed_x,
                          rows = rows,
                          cols = cols,
                          which = which,
                          dim. = dim.,
                          ...)
          }
)
