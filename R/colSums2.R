### ============================================================================
### colSums2
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_colSums2 <- function(x, rows = NULL, cols = NULL,
                                          na.rm = FALSE, dim. = dim(x), ...) {
  # Check input
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = TRUE)

  # Subset
  x <- ..subset(x, rows, cols)

  # Compute result
  val <- DelayedArray:::colblock_APPLY(x = x,
                                       APPLY = matrixStats::colSums2,
                                       na.rm = na.rm,
                                       ...)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  # NOTE: Return value of matrixStats::colSums2() has no names
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
#' @template common_params
#' @template lowercase_x
#' @export
#' @template example_dm_matrix
#' @template example_dm_MatrixMatrix
#' @author Peter Hickey
#' @examples
#'
#' colSums2(dm_matrix)
setMethod("colSums2", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                   force_block_processing = FALSE, ...) {
            if (!hasMethod("colSums2", seedClass(x)) ||
                force_block_processing) {
              message2("Block processing", get_verbose())
              return(.DelayedMatrix_block_colSums2(x = x,
                                                   rows = rows,
                                                   cols = cols,
                                                   na.rm = na.rm,
                                                   dim. = dim.))
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
                return(colSums2(x = x,
                                rows = rows,
                                cols = cols,
                                na.rm = na.rm,
                                dim. = dim.,
                                force_block_processing = TRUE,
                                ...))
              }
            }

            colSums2(x = simple_seed_x,
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

#' @export
setMethod("colSums2", "matrix", matrixStats::colSums2)

#' @importMethodsFrom Matrix colSums
#' @rdname colSums2
#' @export
setMethod("colSums2", "Matrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                   ...) {
            message2(class(x), get_verbose())
            x <- ..subset(x, rows, cols)
            # NOTE: Return value of matrixStats::colSums2() has no names
            unname(colSums(x = x, na.rm = na.rm))
          }
)

#' @importMethodsFrom IRanges Views viewSums
#' @rdname colSums2
#' @export
setMethod("colSums2", "SolidRleArraySeed",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                   ...) {
            message2(class(x), get_verbose())
            irl <- get_Nindex_as_IRangesList(Nindex = list(rows, cols),
                                             dim = dim(x))
            views <- Views(subject = x@rle, start = unlist(irl))
            val <- viewSums(x = views, na.rm = na.rm)
            if (length(irl) == 0) {
              return(val)
            }
            n <- length(irl[[1]])
            if (n == 1) {
              return(val)
            }
            IDX <- rep(seq_along(irl), each = n)
            unlist(lapply(X = split(val, IDX), FUN = sum, na.rm = na.rm),
                   use.names = FALSE)
          }
)
