### ============================================================================
### colTabulates
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_colTabulates <- function(x, rows = NULL, cols = NULL,
                                              values = NULL, ...) {
  # Check input type
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = FALSE)

  # Subset
  x <- ..subset(x, rows, cols)

  # Compute result
  if (is.null(values)) {
    # NOTE: Need to compute values from entire x (not block-by-block)
    values <- sort(
      unique(
        unlist(DelayedArray:::colblock_APPLY(x = x,
                                             APPLY = function(x) {
                                               unique(as.vector(x))
                                             }),
               use.names = FALSE)),
      na.last = TRUE)
  }
  val <- DelayedArray:::colblock_APPLY(x = x,
                                       APPLY = matrixStats::colTabulates,
                                       values = values,
                                       ...)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  # NOTE: Return value of matrixStats::colTabulates() has names
  do.call(rbind, val)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit MatrixGenerics::colTabulates
#' @importFrom MatrixGenerics colTabulates
#' @importMethodsFrom DelayedArray seed
#' @rdname colTabulates
#' @template common_params
#' @template lowercase_x
#' @export
#' @template example_dm_S4VectorsDF
#' @author Peter Hickey
#' @examples
#'
#' colTabulates(dm_DF)
setMethod("colTabulates", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, values = NULL,
                   force_block_processing = FALSE, ...) {
            if (!type(x) %in% c("integer", "logical", "raw")) {
              stop("Argument 'x' is not of type integer, logical, or raw",
                   " (type = ", type(x), ")")
            }
            if (!hasMethod("colTabulates", seedClass(x)) ||
                force_block_processing) {
              message2("Block processing", get_verbose())
              return(.DelayedMatrix_block_colTabulates(x = x,
                                                       rows = rows,
                                                       cols = cols,
                                                       values = values,
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
                return(colTabulates(x = x,
                                    rows = rows,
                                    cols = cols,
                                    values = values,
                                    force_block_processing = TRUE,
                                    ...))
              }
            }

            colTabulates(x = simple_seed_x,
                         rows = rows,
                         cols = cols,
                         values = values,
                         ...)
          }
)

# ------------------------------------------------------------------------------
# Seed-aware methods
#
