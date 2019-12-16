### =============================================================================
### rowSums2
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_rowSums2 <- function(x, rows = NULL, cols = NULL,
                                          na.rm = FALSE, dim. = dim(x), ...) {
  # Check input
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = FALSE)

  # Subset
  x <- ..subset(x, rows, cols)

  # Compute result
  val <- rowblock_APPLY(x = x,
                        APPLY = matrixStats::rowSums2,
                        na.rm = na.rm,
                        ...)
  if (length(val) == 0L) {
    return(numeric(nrow(x)))
  }
  # NOTE: Return value of matrixStats::rowSums() has no names
  unlist(val, recursive = FALSE, use.names = FALSE)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @importMethodsFrom DelayedArray seed
#' @rdname colSums2
#' @export
#' @examples
#'
#' # NOTE: Temporarily use verbose output to demonstrate which method is
#' #       which method is being used
#' options(DelayedMatrixStats.verbose = TRUE)
#' # By default, this uses a seed-aware method for a DelayedMatrix with a
#' # 'SolidRleArraySeed' seed
#' rowSums2(dm_Matrix)
#' # Alternatively, can use the block-processing strategy
#' rowSums2(dm_Matrix, force_block_processing = TRUE)
#' options(DelayedMatrixStats.verbose = FALSE)
setMethod("rowSums2", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                   force_block_processing = FALSE, ...) {
            if (!hasMethod("rowSums2", seedClass(x)) ||
                force_block_processing) {
              message2("Block processing", get_verbose())
              return(.DelayedMatrix_block_rowSums2(x = x,
                                                   rows = rows,
                                                   cols = cols,
                                                   na.rm = na.rm,
                                                   dim. = dim.))
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
                return(rowSums2(x = x,
                                rows = rows,
                                cols = cols,
                                na.rm = na.rm,
                                dim. = dim.,
                                force_block_processing = TRUE,
                                ...))
              }
            }

            rowSums2(x = simple_seed_x,
                     rows = rows,
                     cols = cols,
                     na.rm = na.rm,
                     dim. = dim.,
                     ...)
          }
)

# ------------------------------------------------------------------------------
# Seed-aware methods
#

#' @importMethodsFrom Matrix rowSums
#' @rdname colSums2
#' @export
setMethod("rowSums2", "Matrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                   ...) {
            message2(class(x), get_verbose())
            x <- ..subset(x, rows, cols)
            # NOTE: Return value of matrixStats::rowSums2() has no names
            unname(rowSums(x = x, na.rm = na.rm))
          }
)
