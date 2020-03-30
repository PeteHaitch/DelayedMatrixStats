### ============================================================================
### colMeans2
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_colMeans2 <- function(x, rows = NULL, cols = NULL,
                                           na.rm = FALSE, dim. = dim(x), ...) {
  # Check input
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = TRUE)

  # Subset
  x <- ..subset(x, rows, cols)

  # Compute result
  val <- DelayedArray:::colblock_APPLY(x = x,
                                       APPLY = matrixStats::colMeans2,
                                       na.rm = na.rm,
                                       ...)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  # NOTE: Return value of matrixStats::colMeans2() has no names
  unlist(val, recursive = FALSE, use.names = FALSE)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @importMethodsFrom DelayedArray seed
#' @rdname colMeans2
#' @template common_params
#' @template lowercase_x
#' @export
#' @template example_dm_matrix
#' @template example_dm_Rle
#' @author Peter Hickey
#' @examples
#'
#' colMeans2(dm_matrix)
setMethod("colMeans2", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                   force_block_processing = FALSE, ...) {
            if (!hasMethod("colMeans2", seedClass(x)) ||
                force_block_processing) {
              message2("Block processing", get_verbose())
              return(.DelayedMatrix_block_colMeans2(x = x,
                                                    rows = rows,
                                                    cols = cols,
                                                    na.rm = na.rm,
                                                    dim. = dim.,
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
                return(colMeans2(x = x,
                                 rows = rows,
                                 cols = cols,
                                 na.rm = na.rm,
                                 dim. = dim.,
                                 force_block_processing = TRUE,
                                 ...))
              }
            }

            colMeans2(x = simple_seed_x,
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
setMethod("colMeans2", "matrix", matrixStats::colMeans2)

#' @importMethodsFrom Matrix colMeans
#' @rdname colMeans2
#' @export
setMethod("colMeans2", "Matrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                   ...) {
            message2(class(x), get_verbose())
            x <- ..subset(x, rows, cols)
            # NOTE: Return value of matrixStats::colMeans2() has no names
            unname(colMeans(x = x, na.rm = na.rm))
          }
)

#' @importMethodsFrom IRanges Views viewMeans
#' @rdname colMeans2
#' @export
setMethod("colMeans2", "SolidRleArraySeed",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                   ...) {
            message2(class(x), get_verbose())
            irl <- get_Nindex_as_IRangesList(Nindex = list(rows, cols),
                                             dim = dim(x))
            views <- Views(subject = x@rle, start = unlist(irl))
            val <- viewMeans(x = views, na.rm = na.rm)
            if (length(irl) == 0) {
              return(val)
            }
            n <- length(irl[[1]])
            if (n == 1) {
              return(val)
            }
            IDX <- rep(seq_along(irl), each = n)
            unlist(lapply(X = split(val, IDX), FUN = mean, na.rm = na.rm),
                   use.names = FALSE)
          }
)
