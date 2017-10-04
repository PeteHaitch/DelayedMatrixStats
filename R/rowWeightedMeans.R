### ============================================================================
### rowWeightedMeans
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

#' `rowWeightedMeans()` block-processing internal helper
#' @inherit matrixStats::rowWeightedMeans
.DelayedMatrix_block_rowWeightedMeans <- function(x, w = NULL, rows = NULL,
                                                  cols = NULL, na.rm = FALSE,
                                                  ...) {
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
                        APPLY = matrixStats::rowWeightedMeans,
                        w = w,
                        na.rm = na.rm,
                        ...)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  # NOTE: Return value of matrixStats::rowWeightedMeans() has names
  unlist(val, recursive = FALSE, use.names = TRUE)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @importMethodsFrom DelayedArray seed
#' @rdname colWeightedMeans
#' @export
#' @examples
#' rowWeightedMeans(dm_Matrix, w = 1:3)
setMethod("rowWeightedMeans", "DelayedMatrix",
          function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE,
                   force_block_processing = FALSE, ...) {
            if (!hasMethod("rowWeightedMeans", class(seed(x))) ||
                force_block_processing) {
              message2("Block processing", get_verbose())
              return(.DelayedMatrix_block_rowWeightedMeans(x = x,
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
                return(rowWeightedMeans(x = x,
                                        w = w,
                                        rows = rows,
                                        cols = cols,
                                        na.rm = na.rm,
                                        force_block_processing = TRUE,
                                        ...))
              }
            }

            rowWeightedMeans(x = simple_seed_x,
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

#' @export
setMethod("rowWeightedMeans", "matrix", matrixStats::rowWeightedMeans)
