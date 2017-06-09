#-------------------------------------------------------------------------------
# Non-exported methods (used on a seed **not** on a DelayedMatrix object)
# TODO: These are being exported; why?
#

#' @importFrom matrixStats rowSums2
#' @importFrom methods setMethod
setMethod("rowSums2", "matrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                   ...) {
            message("matrix")
            matrixStats::rowSums2(x, rows, cols, na.rm, dim., ...)
          }
)

# NOTE: No different from rowSums2,ANY-method except that it explicitly calls
#       Matrix::rowSums() to resolve what I think is a namespace collision with
#       BiocGenerics::rowSums()
# TODO: Profile (this probably generates a copy when rows or cols is non NULL)
#' @importFrom methods setMethod
#' @rdname rowSums2
setMethod("rowSums2", "Matrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                   ...) {
            message("Matrix")
            if (!is.null(rows)) {
              x <- x[rows, , drop = FALSE]
            }
            if (!is.null(cols)) {
              x <- x[, cols, drop = FALSE]
            }
            # NOTE: Return value of matrixStats::rowSums2() has no names
            unname(Matrix::rowSums(x, na.rm))
          }
)

# NOTE: This feels a little circular since it takes an ANY (those for which we
#       don't have an explicit rowSums2() method) as a seed, contructs an
#       DelayedMatrix from that seed, subsets the DelayedMatrix, and then calls
#       rowSums,DelayedMatrix-method. All of this is to avoid writing an
#       explicit rowSums2() method for these classes of seeds, for which it is
#       likely to be difficult to write an implementation that is more
#       efficient or simpler than the "block processing" strategy used by
#       rowSums,DelayedMatrix-method
#' @importFrom DelayedArray DelayedArray
#' @importFrom methods setMethod
#' @rdname rowSums2
setMethod("rowSums2", "ANY",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                   ...) {
            message("ANY")
            if (length(x) == 0L) {
              return(numeric(0L))
            }
            x <- DelayedArray::DelayedArray(x)
            if (!is.null(rows)) {
              x <- x[rows, , drop = FALSE]
            }
            if (!is.null(cols)) {
              x <- x[, cols, drop = FALSE]
            }
            # NOTE: Return value of matrixStats::rowSums2() has no names
            unname(rowSums(x, na.rm))
          }
)

#-------------------------------------------------------------------------------
# Exported methods
#

#' @importFrom BiocGenerics rowSums
#' @importFrom DelayedArray seed
#' @importFrom methods is
#' @rdname rowSums2
#' @export
setMethod("rowSums2", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                   ...) {
            if (!.is_simple_seed(seed(x)) || x@is_transposed) {
              message("The rest")
              # Subset and defer to rowSums
              if (!is.null(rows)) {
                x <- x[rows, ]
              }
              if (!is.null(cols)) {
                x <- x[, cols]
              }
              # TODO: Check dims.?
              # NOTE: Return value of matrixStats::rowSums2() has no names
              return(unname(rowSums(x, na.rm)))

            }
            if (DelayedArray:::is_pristine(x)) {
              # TODO: Don't think I need this branch (next suffices?)
              message("Pristine")
              x <- DelayedArray:::remove_pristine_DelayedArray_wrapping(x)
            } else {
              message("Not transposed")
              x <- .from_DelayedArray_to_simple_seed_class(x, FALSE)
            }
            rowSums2(x, rows, cols, na.rm, dim., ...)
          }
)

# TODO: Some of the tricks to avoid unnecessary block processing in rowSums2()
#       are also applicable to rowSums()