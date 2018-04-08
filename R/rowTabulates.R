### ============================================================================
### rowTabulates
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

#' `rowTabulates()` block-processing internal helper
#' @inherit matrixStats::rowTabulates
.DelayedMatrix_block_rowTabulates <- function(x, rows = NULL, cols = NULL,
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
                                       APPLY = matrixStats::rowTabulates,
                                       values = values,
                                       ...)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  # NOTE: Return value of matrixStats::rowTabulates() has names
  Reduce(`+`, val)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @importMethodsFrom DelayedArray seed
#' @rdname colTabulates
#' @export
#' @examples
#'
#' rowTabulates(dm_DF)
setMethod("rowTabulates", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, values = NULL,
                   force_block_processing = FALSE, ...) {
            if (!type(x) %in% c("integer", "logical", "raw")) {
              stop("Argument 'x' is not of type integer, logical, or raw",
                   " (type = ", type(x), ")")
            }
            if (!hasMethod("rowTabulates", seedClass(x)) ||
                force_block_processing) {
              message2("Block processing", get_verbose())
              return(.DelayedMatrix_block_rowTabulates(x = x,
                                                       rows = rows,
                                                       cols = cols,
                                                       values = values,
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
                return(rowTabulates(x = x,
                                    rows = rows,
                                    cols = cols,
                                    values = values,
                                    force_block_processing = TRUE,
                                    ...))
              }
            }

            rowTabulates(x = simple_seed_x,
                         rows = rows,
                         cols = cols,
                         values = values,
                         ...)
          }
)

# ------------------------------------------------------------------------------
# Seed-aware methods
#

#' @export
setMethod("rowTabulates", "matrix", matrixStats::rowTabulates)
