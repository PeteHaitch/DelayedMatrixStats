### ============================================================================
### rowWeightedVars
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

#' `rowWeightedVars()` block-processing internal helper
#' @inherit matrixStats::rowWeightedVars
#' @importFrom methods is
.DelayedMatrix_block_rowWeightedVars <- function(x, w = NULL, rows = NULL,
                                                 cols = NULL, na.rm = FALSE,
                                                 ...) {
  # Check input type
  stopifnot(is(x, "DelayedMatrix"))
  stopifnot(!x@is_transposed)
  DelayedArray:::.get_ans_type(x)

  # Subset
  x <- ..subset(x, rows, cols)
  if (!is.null(w) && !is.null(cols)) {
    w <- w[cols]
  }

  # Compute result
  val <- rowblock_APPLY(x = x,
                        APPLY = matrixStats::rowWeightedVars,
                        w = w,
                        na.rm = na.rm,
                        ...)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  # NOTE: Return value of matrixStats::rowWeightedVars() has names
  unlist(val, recursive = FALSE, use.names = TRUE)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @importFrom DelayedArray seed
#' @importFrom methods hasMethod is
#' @rdname colWeightedVars
#' @export
setMethod("rowWeightedVars", "DelayedMatrix",
          function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE,
                   force_block_processing = FALSE, ...) {
            if (!hasMethod("rowWeightedVars", class(seed(x))) ||
                force_block_processing) {
              message2("Block processing", get_verbose())
              return(.DelayedMatrix_block_rowWeightedVars(x = x,
                                                          w = w,
                                                          rows = rows,
                                                          cols = cols,
                                                          na.rm = na.rm,
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
                return(rowWeightedVars(x = x,
                                       w = w,
                                       rows = rows,
                                       cols = cols,
                                       na.rm = na.rm,
                                       force_block_processing = TRUE,
                                       ...))
              }
            }

            rowWeightedVars(x = simple_seed_x,
                            w = w,
                            rows = rows,
                            cols = cols,
                            na.rm = na.rm,
                            ...)
          }
)

# ------------------------------------------------------------------------------
# Seed-aware methods
#

#' @importFrom methods setMethod
#' @export
setMethod("rowWeightedVars", "matrix", matrixStats::rowWeightedVars)
