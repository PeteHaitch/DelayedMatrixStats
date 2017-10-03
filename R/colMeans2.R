### =============================================================================
### colMeans2
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

#' `colMeans2()` block-processing internal helper
#' @inherit matrixStats::colMeans2
#' @importFrom methods is
.DelayedMatrix_block_colMeans2 <- function(x, rows = NULL, cols = NULL,
                                           na.rm = FALSE, dim. = dim(x), ...) {
  # Check input
  stopifnot(is(x, "DelayedMatrix"))
  stopifnot(!x@is_transposed)
  DelayedArray:::.get_ans_type(x, must.be.numeric = TRUE)

  # Subset
  x <- ..subset(x, rows, cols)

  # Compute result
  # TODO: Use this or colblock_APPLY() with matrixStats::colMeans2()?
  # NOTE: Return value of matrixStats::colMeans2() has no names
  unname(DelayedArray::colMeans(x = x, na.rm = na.rm))
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @importFrom DelayedArray seed
#' @importFrom methods hasMethod is
#' @rdname colMeans2
#' @template common_params
#' @template lowercase_x
#' @export
setMethod("colMeans2", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                   force_block_processing = FALSE, ...) {
            if (!hasMethod("colMeans2", class(seed(x))) ||
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

#' @importFrom methods setMethod
#' @export
setMethod("colMeans2", "matrix", matrixStats::colMeans2)

#' @importFrom methods setMethod
#' @rdname colMeans2
#' @export
setMethod("colMeans2", "Matrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                   ...) {
            message2(class(x), get_verbose())
            x <- ..subset(x, rows, cols)
            # NOTE: Return value of matrixStats::colMeans2() has no names
            unname(Matrix::colMeans(x = x, na.rm = na.rm))
          }
)

#' @importFrom IRanges Views viewSums
#' @rdname colMeans2
#' @export
setMethod("colMeans2", "SolidRleArraySeed",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                   ...) {
            message2(class(x), get_verbose())
            irl <- get_Nindex_as_IRangesList(Nindex = list(rows, cols),
                                             dim = dim(x))
            views <- IRanges::Views(subject = x@rle, start = unlist(irl))
            val <- IRanges::viewMeans(x = views, na.rm = na.rm)
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
