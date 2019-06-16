### ============================================================================
### rowMads
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_rowMads <- function(x, rows = NULL, cols = NULL,
                                         center = NULL, constant = 1.4826,
                                         na.rm = FALSE, dim. = dim(x), ...) {
  # Check input type
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = TRUE)

  # Subset
  x <- ..subset(x, rows, cols)

  # Compute result
  val <- rowblock_APPLY(x = x,
                        APPLY = matrixStats::rowMads,
                        center = center,
                        constant = constant,
                        na.rm = na.rm,
                        ...)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  # NOTE: Return value of matrixStats::rowMads() has no names
  unlist(val, recursive = FALSE, use.names = FALSE)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @importMethodsFrom DelayedArray seed
#' @rdname colMads
#' @export
#' @examples
#'
#' rowMads(dm_DF)
setMethod("rowMads", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, center = NULL,
                   constant = 1.4826, na.rm = FALSE, dim. = dim(x),
                   force_block_processing = FALSE, ...) {
            if (!hasMethod("rowMads", seedClass(x)) ||
                force_block_processing) {
              message2("Block processing", get_verbose())
              return(.DelayedMatrix_block_rowMads(x = x,
                                                  rows = rows,
                                                  cols = cols,
                                                  center = center,
                                                  constant = constant,
                                                  na.rm = na.rm,
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
                return(rowMads(x = x,
                               rows = rows,
                               cols = cols,
                               center = center,
                               constant = constant,
                               na.rm = na.rm,
                               dim. = dim.,
                               force_block_processing = TRUE,
                               ...))
              }
            }

            rowMads(x = simple_seed_x,
                    rows = rows, cols = cols,
                    center = center,
                    constant = constant,
                    na.rm = na.rm,
                    dim. = dim.,
                    ...)
          }
)

# ------------------------------------------------------------------------------
# Seed-aware methods
#

#' @export
setMethod("rowMads", "matrix", matrixStats::rowMads)
