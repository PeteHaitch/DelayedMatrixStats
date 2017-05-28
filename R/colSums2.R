#-------------------------------------------------------------------------------
# Non-exported methods (used on a seed **not** on a DelayedMatrix object)
#

#' @importFrom matrixStats colSums2
#' @importFrom methods setMethod
setMethod(".colSums2", "matrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                   ...) {
            message("matrix")
            matrixStats::colSums2(x, rows, cols, na.rm, dim., ...)
          }
)

# NOTE: No different from .colSums2,ANY-method except that it explicitly calls
#       Matrix::colSums() to resolve what I think is a namespace collision with
#       BiocGenerics::colSums()
# TODO: Profile (this probably generates a copy when rows or cols is non NULL)
#' @importFrom methods setMethod
setMethod(".colSums2", "Matrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                   ...) {
            message("Matrix")
            if (!is.null(rows)) {
              x <- x[rows, , drop = FALSE]
            }
            if (!is.null(cols)) {
              x <- x[, cols, drop = FALSE]
            }
            # NOTE: Return value of matrixStats::colSums2() has no names
            unname(Matrix::colSums(x, na.rm))
          }
)

#' @importFrom IRanges Views viewSums
setMethod(".colSums2", "RleArraySeed",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                   ...) {
            irl <- get_Nindex_as_IRangesList(list(rows, cols), dim(x))
            views <- IRanges::Views(x@rle, unlist(irl))
            val <- IRanges::viewSums(views, na.rm)
            if (length(irl) == 0) {
              return(val)
            }
            n <- length(irl[[1]])
            if (n == 1) {
              return(val)
            }
            IDX <- rep(seq_along(irl), each = n)
            unlist(lapply(split(val, IDX), sum, na.rm), use.names = FALSE)
          }
)

# NOTE: This feels a little circular since it takes an ANY (those for which we
#       don't have an explicit .colSums2() method) as a seed, contructs an
#       DelayedMatrix from that seed, subsets the DelayedMatrix, and then calls
#       colSums,DelayedMatrix-method. All of this is to avoid writing an
#       explicit .colSums2() method for these classes of seeds, for which it is
#       likely to be difficult to write an implementation that is more
#       efficient or simpler than the "block processing" strategy used by
#       colSums,DelayedMatrix-method
#' @importFrom DelayedArray DelayedArray
#' @importFrom methods setMethod
setMethod(".colSums2", "ANY",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                   ...) {
            message("ANY")
            x <- DelayedArray::DelayedArray(x)
            if (!is.null(rows)) {
              x <- x[rows, , drop = FALSE]
            }
            if (!is.null(cols)) {
              x <- x[, cols, drop = FALSE]
            }
            # NOTE: Return value of matrixStats::colSums2() has no names
            unname(colSums(x, na.rm))
          }
)

#-------------------------------------------------------------------------------
# Exported methods
#

#' @importFrom BiocGenerics colSums
#' @importFrom DelayedArray seed
#' @importFrom methods is
#' @rdname colSums2
#' @export
setMethod("colSums2", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                   ...) {
            if (!.is_simple_seed(seed(x)) || x@is_transposed) {
              message("The rest")
              # Subset and defer to colSums
              if (!is.null(rows)) {
                x <- x[rows, ]
              }
              if (!is.null(cols)) {
                x <- x[, cols]
              }
              # TODO: Check dims.?
              # NOTE: Return value of matrixStats::colSums2() has no names
              return(unname(colSums(x, na.rm)))

            }
            if (DelayedArray:::is_pristine(x)) {
              # TODO: Don't think I need this branch (next suffices?)
              message("Pristine")
              x <- DelayedArray:::remove_pristine_DelayedArray_wrapping(x)
            } else {
              message("Not transposed")
              x <- .from_DelayedArray_to_simple_seed_class(x, FALSE)
            }
            .colSums2(x, rows, cols, na.rm, dim., ...)
          }
)
