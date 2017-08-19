### ============================================================================
### colQuantiles
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

#' `colQuantiles()` block-processing internal helper
#' @inherit matrixStats::colQuantiles
#' @importFrom methods is
.DelayedMatrix_block_colQuantiles <-
  function(x, rows = NULL, cols = NULL,
           probs = seq(from = 0, to = 1, by = 0.25), na.rm = FALSE, type = 7L,
           ..., drop = TRUE) {
    # Check input type
    stopifnot(is(x, "DelayedMatrix"))
    stopifnot(!x@is_transposed)
    DelayedArray:::.get_ans_type(x, must.be.numeric = TRUE)

    # Subset
    x <- ..subset(x, rows = rows, cols = cols)

    # Compute result
    val <- DelayedArray:::colblock_APPLY(x,
                                         matrixStats::colQuantiles,
                                         probs = probs,
                                         na.rm = na.rm,
                                         type = type,
                                         ...,
                                         drop = drop)
    if (length(val) == 0L) {
      return(numeric(ncol(x)))
    }
    # NOTE: Return value of matrixStats::colQuantiles() is a vector if input is
    #       a column vector (matrix with 1 column)
    if (ncol(x) == 1L) {
      return(unlist(val))
    }
    val <- do.call(cbind, val)
    # NOTE: Return value of matrixStats::colQuantiles() has colnames if
    #       return value is a matrix and does not have NA/NaN
    if (!any(colAnyNAs(val))) {
      rownames(val) <- colnames(x)
    }
    val
  }

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @importFrom DelayedArray seed
#' @importFrom methods hasMethod is
#' @rdname colQuantiles
#' @template common_params
#' @export
setMethod("colQuantiles", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL,
                   probs = seq(from = 0, to = 1, by = 0.25), na.rm = FALSE,
                   type = 7L, force_block_processing = FALSE, ...,
                   drop = TRUE) {
            if (!hasMethod("colQuantiles", class(seed(x))) ||
                force_block_processing) {
              message2("Block processing", get_verbose())
              return(.DelayedMatrix_block_colQuantiles(x, rows, cols, probs,
                                                       na.rm, type, ...,
                                                       drop = drop))
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
                return(colQuantiles(x, rows, cols, probs, na.rm, type,
                                    force_block_processing = TRUE, ...,
                                    drop = drop))
              }
            }

            colQuantiles(simple_seed_x, rows, cols, probs, na.rm, type, ...,
                         drop = drop)
          }
)

# ------------------------------------------------------------------------------
# Seed-aware methods
#

#' @importFrom methods setMethod
#' @export
setMethod("colQuantiles", "matrix", matrixStats::colQuantiles)
