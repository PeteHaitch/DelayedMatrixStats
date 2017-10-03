### ============================================================================
### rowRanks
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

#' `rowRanks()` block-processing internal helper
#' @inherit matrixStats::rowRanks
#' @importFrom methods is
.DelayedMatrix_block_rowRanks <-
  function(x, rows = NULL, cols = NULL,
           ties.method = c("max", "average", "min"), dim. = dim(x), ...) {
    # Check input type
    ties.method <- match.arg(ties.method)
    stopifnot(is(x, "DelayedMatrix"))
    stopifnot(!x@is_transposed)
    DelayedArray:::.get_ans_type(x, must.be.numeric = TRUE)

    # Subset
    x <- ..subset(x, rows, cols)

    # Compute result
    val <- rowblock_APPLY(x = x,
                          APPLY = matrixStats::rowRanks,
                          ties.method = ties.method,
                          ...)
    if (length(val) == 0L) {
      return(numeric(ncol(x)))
    }
    # NOTE: Return value of matrixStats::rowRanks() has no names
    unname(do.call(rbind, val))
  }

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @importFrom DelayedArray seed
#' @importFrom methods hasMethod is
#' @rdname colRanks
#' @export
setMethod("rowRanks", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL,
                   ties.method = c("max", "average", "min"), dim. = dim(x),
                   force_block_processing = FALSE, ...) {
            ties.method <- match.arg(ties.method)
            if (!hasMethod("rowRanks", class(seed(x))) ||
                force_block_processing) {
              message2("Block processing", get_verbose())
              return(
                .DelayedMatrix_block_rowRanks(x = x,
                                              rows = rows,
                                              cols = cols,
                                              ties.method = ties.method,
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
                return(rowRanks(x = x,
                                rows = rows,
                                cols = cols,
                                ties.method = ties.method,
                                dim. = dim.,
                                force_block_processing = TRUE,
                                ...))
              }
            }

            rowRanks(x = simple_seed_x,
                     rows = rows,
                     cols = cols,
                     ties.method = ties.method,
                     dim. = dim.,
                     ...)
          }
)

# ------------------------------------------------------------------------------
# Seed-aware methods
#

#' @importFrom methods setMethod
#' @export
setMethod("rowRanks", "matrix", matrixStats::rowRanks)
