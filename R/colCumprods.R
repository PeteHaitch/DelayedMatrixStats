### ============================================================================
### colCumprods
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

#' `colCumprods()` block-processing internal helper
#' @inherit matrixStats::colCumprods
.DelayedMatrix_block_colCumprods <- function(x, rows = NULL, cols = NULL,
                                             dim. = dim(x), ...) {
  # Check input type
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = TRUE)

  # Subset
  x <- ..subset(x, rows, cols)

  # Compute result
  val <- DelayedArray:::colblock_APPLY(x = x,
                                       APPLY = matrixStats::colCumprods,
                                       ...)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  # NOTE: Return value of matrixStats::colCumprods() has no names
  unname(do.call(cbind, val))
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @importMethodsFrom DelayedArray seed
#' @rdname colCummaxs
#' @export
#' @examples
#'
#' colCumprods(dm_matrix)
setMethod("colCumprods", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, dim. = dim(x),
                   force_block_processing = FALSE, ...) {
            if (!hasMethod("colCumprods", class(seed(x))) ||
                force_block_processing) {
              message2("Block processing", get_verbose())
              return(.DelayedMatrix_block_colCumprods(x = x,
                                                      rows = rows,
                                                      cols = cols,
                                                      dim. = dim.,
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
                return(colCumprods(x = x,
                                   rows = rows,
                                   cols = cols,
                                   dim. = dim.,
                                   force_block_processing = TRUE,
                                   ...))
              }
            }

            colCumprods(x = simple_seed_x,
                        rows = rows,
                        cols = cols,
                        dim. = dim.,
                        ...)
          }
)

# ------------------------------------------------------------------------------
# Seed-aware methods
#

#' @export
setMethod("colCumprods", "matrix", matrixStats::colCumprods)
