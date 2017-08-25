### =============================================================================
### rowSums2
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

#' `rowSums2()` block-processing internal helper
#' @inherit matrixStats::rowSums2
#' @importFrom methods is
.DelayedMatrix_block_rowSums2 <- function(x, rows = NULL, cols = NULL,
                                          na.rm = FALSE, dim. = dim(x), ...) {
  # Check input
  stopifnot(is(x, "DelayedMatrix"))
  stopifnot(!x@is_transposed)
  DelayedArray:::.get_ans_type(x)

  # Subset
  x <- ..subset(x, rows, cols)

  # Compute result
  # TODO: Use this or rowblock_APPLY() with matrixStats::rowSums2()?
  val <- DelayedArray::rowSums(x = x, na.rm = na.rm)

  # NOTE: Return value of matrixStats::rowSums2() has no names
  unname(val)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @importFrom DelayedArray seed
#' @importFrom methods hasMethod is
#' @rdname colSums2
#' @export
setMethod("rowSums2", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                   force_block_processing = FALSE, ...) {
            if (!hasMethod("rowSums2", class(seed(x))) ||
                force_block_processing) {
              message2("Block processing", get_verbose())
              return(.DelayedMatrix_block_rowSums2(x = x,
                                                   rows = rows,
                                                   cols = cols,
                                                   na.rm = na.rm,
                                                   dim. = dim.))
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
                return(rowSums2(x = x,
                                rows = rows,
                                cols = cols,
                                na.rm = na.rm,
                                dim. = dim.,
                                force_block_processing = TRUE,
                                ...))
              }
            }

            rowSums2(x = simple_seed_x,
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
setMethod("rowSums2", "matrix", matrixStats::rowSums2)

#' @importFrom methods setMethod
#' @rdname colSums2
#' @export
setMethod("rowSums2", "Matrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                   ...) {
            message2(class(x), get_verbose())
            x <- ..subset(x, rows, cols)
            # NOTE: Return value of matrixStats::rowSums2() has no names
            unname(Matrix::rowSums(x = x, na.rm = na.rm))
          }
)
