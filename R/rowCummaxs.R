### ============================================================================
### rowCummaxs
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_rowCummaxs <- function(x, rows = NULL, cols = NULL,
                                            dim. = dim(x), ...) {
  # Check input type
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = TRUE)

  # Subset
  x <- ..subset(x, rows, cols)

  # Compute result
  val <- rowblock_APPLY(x = x,
                        APPLY = matrixStats::rowCummaxs,
                        ...)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  # NOTE: Return value of matrixStats::rowCummaxs() has no names
  unname(do.call(rbind, val))
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @importMethodsFrom DelayedArray seed
#' @rdname colCummaxs
#' @export
#' @examples
#'
#' # Only use rows 2-4
#' rowCummaxs(dm_Matrix, rows = 2:4)
setMethod("rowCummaxs", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, dim. = dim(x),
                   force_block_processing = FALSE, ...) {
            if (!hasMethod("rowCummaxs", seedClass(x)) ||
                force_block_processing) {
              message2("Block processing", get_verbose())
              return(.DelayedMatrix_block_rowCummaxs(x = x,
                                                     rows = rows,
                                                     cols = cols,
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
                return(rowCummaxs(x = x,
                                  rows = rows,
                                  cols = cols,
                                  dim. = dim.,
                                  force_block_processing = TRUE,
                                  ...))
              }
            }

            rowCummaxs(x = simple_seed_x,
                       rows = rows,
                       cols = cols,
                       dim. = dim.,
                       ...)
          }
)

# ------------------------------------------------------------------------------
# Seed-aware methods
#

#' @export
setMethod("rowCummaxs", "matrix", matrixStats::rowCummaxs)
