### =============================================================================
### colProds
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

#' `colProds()` block-processing internal helper
#' @inherit matrixStats::colProds
#' @importFrom methods is
.DelayedMatrix_block_colProds <- function(x, rows = NULL, cols = NULL,
                                          na.rm = FALSE,
                                          method = c("direct", "expSumLog"),
                                          ...) {
  # Check input
  method <- match.arg(method)
  stopifnot(is(x, "DelayedMatrix"))
  stopifnot(!x@is_transposed)
  DelayedArray:::.get_ans_type(x, must.be.numeric = TRUE)

  # Subset
  x <- ..subset(x, rows = rows, cols = cols)

  # Compute result
  val <- DelayedArray:::colblock_APPLY(x,
                                       matrixStats::colProds,
                                       na.rm = na.rm,
                                       method = method,
                                       ...)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  # NOTE: Return value of matrixStats::colProds() has no names
  unlist(val, recursive = FALSE, use.names = FALSE)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @importFrom BiocGenerics colSums
#' @importFrom DelayedArray seed
#' @importFrom methods hasMethod is
#' @rdname colProds
#' @template common_params
#' @export
setMethod("colProds", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE,
                   method = c("direct", "expSumLog"),
                   force_block_processing = FALSE, ...) {
            method <- match.arg(method)
            if (!hasMethod("colProds", class(seed(x))) ||
                force_block_processing) {
              message2("Block processing", get_verbose())
              return(.DelayedMatrix_block_colProds(x, rows, cols, na.rm,
                                                   method))
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
                return(colProds(x, rows, cols, na.rm, method,
                                force_block_processing = TRUE, ...))
              }
            }

            colProds(simple_seed_x, rows, cols, na.rm, method, ...)
          }
)

# ------------------------------------------------------------------------------
# Seed-aware methods
#

#' @importFrom methods setMethod
#' @export
setMethod("colProds", "matrix", matrixStats::colProds)

#' @importFrom IRanges Views viewApply
#' @rdname colProds
#' @export
setMethod("colProds", "SolidRleArraySeed",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE,
                   method = c("direct", "expSumLog"), ...) {
            method <- match.arg(method)
            if (method != "direct") {
              stop("Only the 'direct' method is currently supported for ",
                   "DelayedMatrix with '", class(x), "' seed.")
            }
            message2(class(x), get_verbose())
            irl <- get_Nindex_as_IRangesList(list(rows, cols), dim(x))
            views <- IRanges::Views(x@rle, unlist(irl))
            val <- IRanges::viewApply(views, prod, na.rm = na.rm)
            if (length(irl) == 0) {
              return(numeric(ncol(x)))
            }
            n <- length(irl[[1]])
            if (n == 1) {
              return(val)
            }
            IDX <- rep(seq_along(irl), each = n)
            unlist(lapply(split(val, IDX), prod, na.rm = na.rm),
                   use.names = FALSE)
          }
)
