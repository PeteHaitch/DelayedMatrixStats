### =============================================================================
### rowMeans2
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

#' `rowMeans2()` block-processing internal helper
#' @inherit matrixStats::rowMeans2
#' @importFrom methods is
.DelayedMatrix_block_rowMeans2 <- function(x, rows = NULL, cols = NULL,
                                           na.rm = FALSE, dim. = dim(x), ...) {
  # Check input
  stopifnot(is(x, "DelayedMatrix"))
  stopifnot(!x@is_transposed)
  DelayedArray:::.get_ans_type(x)

  # Subset
  x <- ..subset(x, rows, cols)

  # Compute result
  if (nrow(x) == 0) {
    # TODO: This is a workaround a bug in DelayedArray::rowMeans(); e.g.m
    #       DelayedArray::rowMeans(DelayedArray(matrix(nrow = 0, ncol = 10)))
    #       gives an error whereas
    #       DelayedArray::colMeans(DelayedArray(matrix(nrow = 10, ncol = 0)))
    #       gives the expected result
    return(numeric(0))
  }
  # TODO: Use this or rowblock_APPLY() with matrixStats::rowMeans2()?
  # NOTE: Return value of matrixStats::rowMeans2() has no names
  unname(DelayedArray::rowMeans(x = x, na.rm = na.rm))
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @importFrom DelayedArray seed
#' @importFrom methods hasMethod is
#' @rdname colMeans2
#' @export
setMethod("rowMeans2", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                   force_block_processing = FALSE, ...) {
            if (!hasMethod("rowMeans2", class(seed(x))) ||
                force_block_processing) {
              message2("Block processing", get_verbose())
              return(.DelayedMatrix_block_rowMeans2(x = x,
                                                    rows = rows,
                                                    cols = cols,
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
                return(rowMeans2(x = x,
                                 rows = rows,
                                 cols = cols,
                                 na.rm = na.rm,
                                 dim. = dim.,
                                 force_block_processing = TRUE,
                                 ...))
              }
            }

            rowMeans2(x = simple_seed_x,
                      rows = rows,
                      cols = cols,
                      na.rm = na.rm,
                      dim. = dim.,
                      ...)
          }
)

# ------------------------------------------------------------------------------
# Seed-aware methods
#

#' @importFrom methods setMethod
#' @export
setMethod("rowMeans2", "matrix", matrixStats::rowMeans2)

#' @importFrom methods setMethod
#' @rdname colMeans2
#' @export
setMethod("rowMeans2", "Matrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                   ...) {
            message2(class(x), get_verbose())
            x <- ..subset(x, rows, cols)
            # NOTE: Return value of matrixStats::rowMeans2() has no names
            unname(Matrix::rowMeans(x = x, na.rm = na.rm))
          }
)
