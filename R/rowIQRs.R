### ============================================================================
### rowIQRs
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_rowIQRs <- function(x, rows = NULL, cols = NULL,
                                         na.rm = FALSE, ...) {
  # Check input type
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = FALSE)

  # Subset
  x <- ..subset(x, rows, cols)

  # Compute result
  val <- rowblock_APPLY(x = x,
                        APPLY = matrixStats::rowIQRs,
                        na.rm = na.rm,
                        ...)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  # NOTE: Return value of matrixStats::rowIQRs() has no names
  unlist(val, recursive = FALSE, use.names = FALSE)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit MatrixGenerics::rowIQRs
#' @importMethodsFrom DelayedArray seed
#' @rdname colIQRs
#' @export
#' @examples
#'
#' # Only using rows 2-4
#' rowIQRs(dm_matrix, rows = 2:4)
setMethod("rowIQRs", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE,
                   force_block_processing = FALSE, ...) {
            if (!hasMethod("rowIQRs", seedClass(x)) ||
                force_block_processing) {
              message2("Block processing", get_verbose())
              return(.DelayedMatrix_block_rowIQRs(x = x,
                                                  rows = rows,
                                                  cols = cols,
                                                  na.rm = na.rm,
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
                return(rowIQRs(x = x,
                               rows = rows,
                               cols = cols,
                               na.rm = na.rm,
                               force_block_processing = TRUE,
                               ...))
              }
            }

            rowIQRs(x = simple_seed_x,
                    rows = rows,
                    cols = cols,
                    na.rm = na.rm,
                    ...)
          }
)
