### ============================================================================
### colMads
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_colMads <- function(x, rows = NULL, cols = NULL,
                                         center = NULL, constant = 1.4826,
                                         na.rm = FALSE, dim. = dim(x), ...) {
  # Check input type
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = TRUE)

  # Subset
  x <- ..subset(x, rows, cols)

  # Compute result
  val <- DelayedArray:::colblock_APPLY(x = x,
                                       APPLY = matrixStats::colMads,
                                       center = center,
                                       constant = constant,
                                       na.rm = na.rm,
                                       ...)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  # NOTE: Return value of matrixStats::colMads() has no names
  unlist(val, recursive = FALSE, use.names = FALSE)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit MatrixGenerics::colMads
#' @importMethodsFrom DelayedArray seed
#' @rdname colMads
#' @template common_params
#' @template lowercase_x
#' @export
#' @template example_dm_df
#' @template example_dm_S4VectorsDF
#' @author Peter Hickey
#' @examples
#'
#' colMads(dm_df)
setMethod("colMads", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, center = NULL,
                   constant = 1.4826, na.rm = FALSE, dim. = dim(x),
                   force_block_processing = FALSE, ...) {
            if (!hasMethod("colMads", seedClass(x)) ||
                force_block_processing) {
              message2("Block processing", get_verbose())
              return(.DelayedMatrix_block_colMads(x = x,
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
                return(colMads(x = x,
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

            colMads(x = simple_seed_x,
                    rows = rows, cols = cols,
                    center = center,
                    constant = constant,
                    na.rm = na.rm,
                    dim. = dim.,
                    ...)
          }
)
