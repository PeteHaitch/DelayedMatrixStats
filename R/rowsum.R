### =============================================================================
### rowsum
###

# TODO: Add `rows` and `cols` args?
.DelayedMatrix_block_rowsum <- function(x, group, reorder = TRUE, ...) {
  # Check input
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = TRUE)

  # Compute result
  val <- rowblock_APPLY(x = x,
                        APPLY = base::rowsum.default,
                        group = group,
                        reorder = reorder,
                        ...)
  if (length(val) == 0L) {
    return(numeric(nrow(x)))
  }
  do.call("rbind", val)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @importMethodsFrom DelayedArray seed
#' @rdname colsum
#' @export
#' @examples
#'
#' rowsum(dm_HDF5, group = c(1, 1, 1, 2, 2))
setMethod("rowsum", "DelayedMatrix",
          function(x, group, reorder = TRUE, force_block_processing = FALSE,
                   ...) {
            if (!existsMethod("rowsum", seedClass(x)) ||
                force_block_processing) {
              message2("Block processing", get_verbose())
              return(.DelayedMatrix_block_rowsum(x = x,
                                                 group = group,
                                                 reorder = reorder,
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
                return(rowsum(x = x,
                              group = group,
                              reorder = reorder,
                              force_block_processing = TRUE,
                              ...))
              }
            }

            rowsum(x = simple_seed_x,
                   group = group,
                   reorder = reorder,
                   ...)
          }
)

# ------------------------------------------------------------------------------
# Seed-aware methods
#

#' @rdname colsum
#' @export
setMethod("rowsum", "matrix", base::rowsum)
