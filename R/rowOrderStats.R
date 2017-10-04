### ============================================================================
### rowOrderStats
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

#' `rowOrderStats()` block-processing internal helper
#' @inherit matrixStats::rowOrderStats
.DelayedMatrix_block_rowOrderStats <- function(x, rows = NULL, cols = NULL,
                                               which, dim. = dim(x), ...) {
  # Check input type
  stopifnot(is(x, "DelayedMatrix"))
  stopifnot(!x@is_transposed)
  DelayedArray:::.get_ans_type(x, must.be.numeric = TRUE)

  # Subset
  x <- ..subset(x, rows, cols)

  # Compute result
  val <- rowblock_APPLY(x = x,
                        APPLY = matrixStats::rowOrderStats,
                        which = which,
                        ...)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  # NOTE: Return value of matrixStats::rowOrderStats() has no names
  unlist(val, recursive = FALSE, use.names = FALSE)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @importMethodsFrom DelayedArray seed
#' @rdname colOrderStats
#' @export
#' @examples
#'
#' # Different algorithms, specified by `which`, may give different results
#' rowOrderStats(dm_Matrix, which = 1)
#' rowOrderStats(dm_Matrix, which = 2)
setMethod("rowOrderStats", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, which, dim. = dim(x),
                   force_block_processing = FALSE, ...) {
            if (!hasMethod("rowOrderStats", class(seed(x))) ||
                force_block_processing) {
              message2("Block processing", get_verbose())
              return(.DelayedMatrix_block_rowOrderStats(x = x,
                                                        rows = rows,
                                                        cols = cols,
                                                        which = which,
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
                return(rowOrderStats(x = x,
                                     rows = rows,
                                     cols = cols,
                                     which = which,
                                     dim. = dim.,
                                     force_block_processing = TRUE,
                                     ...))
              }
            }

            rowOrderStats(x = simple_seed_x,
                          rows = rows,
                          cols = cols,
                          which = which,
                          dim. = dim.,
                          ...)
          }
)

# ------------------------------------------------------------------------------
# Seed-aware methods
#

#' @export
setMethod("rowOrderStats", "matrix", matrixStats::rowOrderStats)
