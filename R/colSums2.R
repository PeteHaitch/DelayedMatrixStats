### =============================================================================
### colSums2
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

#' `colSums2()` block-processing internal helper
#' @inherit matrixStats::colSums2
#' @importFrom methods is
.DelayedMatrix_block_colSums2 <- function(x, rows = NULL, cols = NULL,
                                          na.rm = FALSE, dim. = dim(x), ...) {
  # Check input
  stopifnot(is(x, "DelayedMatrix"))
  stopifnot(!x@is_transposed)
  DelayedArray:::.get_ans_type(x)

  # Subset
  x <- ..subset(x, rows, cols)

  # Compute result
  # TODO: Use this or colblock_APPLY() with matrixStats::colSums2()?
  val <- DelayedArray::colSums(x = x, na.rm = na.rm)

  # NOTE: Return value of matrixStats::colSums2() has no names
  unname(val)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @importFrom DelayedArray seed
#' @importFrom methods hasMethod is
#' @rdname colSums2
#' @template common_params
#' @export
setMethod("colSums2", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                   force_block_processing = FALSE, ...) {
            if (!hasMethod("colSums2", class(seed(x))) ||
                force_block_processing) {
              message2("Block processing", get_verbose())
              return(.DelayedMatrix_block_colSums2(x = x,
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

#' @importFrom methods setMethod
#' @export
setMethod("colSums2", "matrix", matrixStats::colSums2)

#' @importFrom methods setMethod
#' @rdname colSums2
#' @export
setMethod("colSums2", "Matrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                   ...) {
            message2(class(x), get_verbose())
            x <- ..subset(x, rows, cols)
            # NOTE: Return value of matrixStats::colSums2() has no names
            unname(Matrix::colSums(x = x, na.rm = na.rm))
          }
)

#' @importFrom IRanges Views viewSums
#' @rdname colSums2
#' @export
setMethod("colSums2", "SolidRleArraySeed",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                   ...) {
            message2(class(x), get_verbose())
            irl <- get_Nindex_as_IRangesList(Nindex = list(rows, cols),
                                             dim = dim(x))
            views <- IRanges::Views(subject = x@rle, start = unlist(irl))
            val <- IRanges::viewSums(x = views, na.rm = na.rm)
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

#' @importFrom methods setMethod
#' @rdname colSums2
#' @export
setMethod("colSums2", "HDF5Matrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                   ...) {
            warning("colSums2,HDF5Matrix-method not yet properly implemented")
            message2(class(x), get_verbose())
            # NOTE: Return value of matrixStats::colSums2() has no names
            # TODO: What's the most efficient way to convert R-based indexing
            #       (1-based) to C-based indexing (0-based). Currently,
            #       creating an intermediate vector of size length(cols) and,
            #       furthermore, this won't work for the default cols = NULL
            .Call(cxx_colSums2, x, cols - 1L, na.rm)
          }
)
