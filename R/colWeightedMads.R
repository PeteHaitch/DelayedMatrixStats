### ============================================================================
### colWeightedMads
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_colWeightedMads <- function(x, w = NULL, rows = NULL,
                                                 cols = NULL, na.rm = FALSE,
                                                 constant = 1.4826,
                                                 center = NULL, ...) {
  # Check input type
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = FALSE)

  # Subset
  x <- ..subset(x, rows, cols)
  if (!is.null(w) && !is.null(rows)) {
    w <- w[rows]
  }

  # Compute result
  val <- DelayedArray:::colblock_APPLY(x = x,
                                       APPLY = matrixStats::colWeightedMads,
                                       w = w,
                                       na.rm = na.rm,
                                       constant = constant,
                                       center = center,
                                       ...)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  # NOTE: Return value of matrixStats::colWeightedMads() has names
  unlist(val, recursive = FALSE, use.names = TRUE)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit MatrixGenerics::colWeightedMads
#' @importMethodsFrom DelayedArray seed
#' @rdname colWeightedMads
#' @template common_params
#' @template lowercase_x
#' @export
#' @template example_dm_matrix
#' @author Peter Hickey
#' @examples
#'
#' colWeightedMads(dm_matrix, w = 1:5)
setMethod("colWeightedMads", "DelayedMatrix",
          function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE,
                   constant = 1.4826, center = NULL,
                   force_block_processing = FALSE, ...) {
            if (!hasMethod("colWeightedMads", seedClass(x)) ||
                force_block_processing) {
              message2("Block processing", get_verbose())
              return(.DelayedMatrix_block_colWeightedMads(x = x,
                                                          w = w,
                                                          rows = rows,
                                                          cols = cols,
                                                          na.rm = na.rm,
                                                          constant = constant,
                                                          center = center,
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
                return(colWeightedMads(x = x,
                                       w = w,
                                       rows = rows,
                                       cols = cols,
                                       na.rm = na.rm,
                                       constant = constant,
                                       center = center,
                                       force_block_processing = TRUE,
                                       ...))
              }
            }

            colWeightedMads(x = simple_seed_x,
                            w = w,
                            rows = rows,
                            cols = cols,
                            na.rm = na.rm,
                            constant = constant,
                            center = center,
                            ...)
          }
)
