### ============================================================================
### rowVars
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_rowVars <- function(x, rows = NULL, cols = NULL,
                                         na.rm = FALSE, center = NULL,
                                         dim. = dim(x), ...) {
  # Check input type
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = TRUE)

  # Subset
  x <- ..subset(x, rows, cols)

  # Compute result
  val <- rowblock_APPLY(x = x,
                        APPLY = matrixStats::rowVars,
                        na.rm = na.rm,
                        center = center,
                        ...)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  # NOTE: Return value of matrixStats::rowVars() has no names
  unlist(val, recursive = FALSE, use.names = FALSE)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit MatrixGenerics::rowVars
#' @importFrom MatrixGenerics rowVars
#' @importMethodsFrom DelayedArray seed
#' @rdname colVars
#' @export
#' @examples
#'
#' rowVars(dm_matrix)
setMethod("rowVars", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, center = NULL,
                   dim. = dim(x), force_block_processing = FALSE, ...) {
            if (!hasMethod("rowVars", seedClass(x)) ||
                force_block_processing) {
              message2("Block processing", get_verbose())
              return(.DelayedMatrix_block_rowVars(x = x,
                                                  rows = rows,
                                                  cols = cols,
                                                  na.rm = na.rm,
                                                  center = center,
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
                return(rowVars(x = x,
                               rows = rows,
                               cols = cols,
                               na.rm = na.rm,
                               center = center,
                               dim. = dim.,
                               force_block_processing = TRUE,
                               ...))
              }
            }

            rowVars(x = simple_seed_x,
                    rows = rows,
                    cols = cols,
                    na.rm = na.rm,
                    center = center,
                    dim. = dim.,
                    ...)
          }
)
