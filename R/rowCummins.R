### ============================================================================
### rowCummins
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

#' `rowCummins()` block-processing internal helper
#' @inherit matrixStats::rowCummins
#' @importFrom methods is
.DelayedMatrix_block_rowCummins <- function(x, rows = NULL, cols = NULL,
                                            dim. = dim(x), ...) {
  # Check input type
  stopifnot(is(x, "DelayedMatrix"))
  stopifnot(!x@is_transposed)
  DelayedArray:::.get_ans_type(x)

  # Subset
  x <- ..subset(x, rows, cols)

  # Compute result
  val <- rowblock_APPLY(x = x,
                        APPLY = matrixStats::rowCummins,
                        dim. = dim(x),
                        ...)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  # NOTE: Return value of matrixStats::rowCummins() has no names
  unname(do.call(cbind, val))
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @importFrom DelayedArray seed
#' @importFrom methods hasMethod is
#' @rdname rowCummins
#' @template common_params
#' @export
setMethod("rowCummins", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, dim. = dim(x),
                   force_block_processing = FALSE, ...) {
            if (!hasMethod("rowCummins", class(seed(x))) ||
                force_block_processing) {
              message2("Block processing", get_verbose())
              return(.DelayedMatrix_block_rowCummins(x = x,
                                                     rows = rows,
                                                     cols = cols,
                                                     dim. = dim., ...))
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
                return(rowCummins(x = x,
                                  rows = rows,
                                  cols = cols,
                                  dim. = dim.,
                                  force_block_processing = TRUE,
                                  ...))
              }
            }

            rowCummins(x = simple_seed_x,
                       rows = rows,
                       cols = cols,
                       dim. = dim., ...)
          }
)

# ------------------------------------------------------------------------------
# Seed-aware methods
#

#' @importFrom methods setMethod
#' @export
setMethod("rowCummins", "matrix", matrixStats::rowCummins)
