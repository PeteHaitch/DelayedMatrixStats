### ============================================================================
### rowWeightedMads
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

#' `rowWeightedMads()` block-processing internal helper
#' @inherit matrixStats::rowWeightedMads
.DelayedMatrix_block_rowWeightedMads <- function(x, w = NULL, rows = NULL,
                                                 cols = NULL, na.rm = FALSE,
                                                 constant = 1.4826,
                                                 center = NULL, ...) {
  # Check input type
  stopifnot(is(x, "DelayedMatrix"))
  stopifnot(!x@is_transposed)
  DelayedArray:::.get_ans_type(x, must.be.numeric = FALSE)

  # Subset
  x <- ..subset(x, rows, cols)
  if (!is.null(w) && !is.null(cols)) {
    w <- w[cols]
  }

  # Compute result
  val <- rowblock_APPLY(x = x,
                        APPLY = matrixStats::rowWeightedMads,
                        w = w,
                        na.rm = na.rm,
                        constant = constant,
                        center = center,
                        ...)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  # NOTE: Return value of matrixStats::rowWeightedMads() has names
  unlist(val, recursive = FALSE, use.names = TRUE)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @importMethodsFrom DelayedArray seed
#' @rdname colWeightedMads
#' @export
#' @examples
#'
#' rowWeightedMads(dm_matrix, w = 3:1)
setMethod("rowWeightedMads", "DelayedMatrix",
          function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE,
                   constant = 1.4826, center = NULL,
                   force_block_processing = FALSE, ...) {
            if (!hasMethod("rowWeightedMads", class(seed(x))) ||
                force_block_processing) {
              message2("Block processing", get_verbose())
              return(.DelayedMatrix_block_rowWeightedMads(x = x,
                                                          w = w,
                                                          rows = rows,
                                                          cols = cols,
                                                          na.rm = na.rm,
                                                          constant = constant,
                                                          center = center,
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
                return(rowWeightedMads(x = x,
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

            rowWeightedMads(x = simple_seed_x,
                            w = w,
                            rows = rows,
                            cols = cols,
                            na.rm = na.rm,
                            constant = constant,
                            center = center,
                            ...)
          }
)

# ------------------------------------------------------------------------------
# Seed-aware methods
#

#' @export
setMethod("rowWeightedMads", "matrix", matrixStats::rowWeightedMads)
