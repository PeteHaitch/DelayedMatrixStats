### =============================================================================
### rowProds
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

#' `rowProds()` block-processing internal helper
#' @inherit matrixStats::rowProds
#' @importFrom methods is
.DelayedMatrix_block_rowProds <- function(x, rows = NULL, cols = NULL,
                                          na.rm = FALSE,
                                          method = c("direct", "expSumLog"),
                                          ...) {
  # Check input
  method <- match.arg(method)
  stopifnot(is(x, "DelayedMatrix"))
  stopifnot(!x@is_transposed)
  DelayedArray:::.get_ans_type(x, must.be.numeric = FALSE)

  # Subset
  x <- ..subset(x, rows, cols)

  # Compute result
  val <- rowblock_APPLY(x = x,
                        APPLY = matrixStats::rowProds,
                        na.rm = na.rm,
                        method = method,
                        ...)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  # NOTE: Return value of matrixStats::rowProds() has no names
  unlist(val, recursive = FALSE, use.names = FALSE)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @importFrom DelayedArray seed
#' @importFrom methods hasMethod is
#' @rdname colProds
#' @export
setMethod("rowProds", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE,
                   method = c("direct", "expSumLog"),
                   force_block_processing = FALSE, ...) {
            method <- match.arg(method)
            if (!hasMethod("rowProds", class(seed(x))) ||
                force_block_processing) {
              message2("Block processing", get_verbose())
              return(.DelayedMatrix_block_rowProds(x = x,
                                                   rows = rows,
                                                   cols = cols,
                                                   na.rm = na.rm,
                                                   method = method))
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
                return(rowProds(x = x,
                                rows = rows,
                                cols = cols,
                                na.rm = na.rm,
                                method = method,
                                force_block_processing = TRUE,
                                ...))
              }
            }

            rowProds(x = simple_seed_x,
                     rows = rows,
                     cols = cols,
                     na.rm = na.rm,
                     method = method,
                     ...)
          }
)

# ------------------------------------------------------------------------------
# Seed-aware methods
#

#' @importFrom methods setMethod
#' @export
setMethod("rowProds", "matrix", matrixStats::rowProds)
