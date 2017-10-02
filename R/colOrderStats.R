### ============================================================================
### colOrderStats
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

#' `colOrderStats()` block-processing internal helper
#' @inherit matrixStats::colOrderStats
#' @importFrom methods is
.DelayedMatrix_block_colOrderStats <- function(x, rows = NULL, cols = NULL,
                                               which, dim. = dim(x), ...) {
  # Check input type
  stopifnot(is(x, "DelayedMatrix"))
  stopifnot(!x@is_transposed)
  DelayedArray:::.get_ans_type(x)

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

#' @importFrom DelayedArray seed
#' @importFrom methods hasMethod is
#' @rdname colOrderStats
#' @template common_params
#' @template lowercase_x
#' @export
setMethod("colOrderStats", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, which, dim. = dim(x),
                   force_block_processing = FALSE, ...) {
            if (!hasMethod("colOrderStats", class(seed(x))) ||
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

# ------------------------------------------------------------------------------
# Seed-aware methods
#

#' @importFrom methods setMethod
#' @export
setMethod("colOrderStats", "matrix", matrixStats::colOrderStats)
