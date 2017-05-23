#-------------------------------------------------------------------------------
# Non-exported methods (used on a seed **not** on a DelayedMatrix object)
#

# TODO: These are being picked up by R CMD check as undocumented S4 methods.
#       These shouldn't be exported, so shouldn't need documenting. I think
#       that these *are* being exported because the generic is exported. If
#       this is unavoidable, then I will need to write a internal generic,
#       .rowSums2(), that has methods for seeds and reserve rowSums2() for
#       DelayedMatrix objects

#' @importFrom matrixStats rowSums2
#' @importFrom methods setMethod
setMethod(".rowSums2", "matrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                   ...) {
            message("matrix")
            matrixStats::rowSums2(x, rows, cols, na.rm, dim., ...)
          }
)

# NOTE: No different from rowSums2,ANY-method except that it explicitly calls
#       Matrix::rowSums() to resolve what I think is a namespace collision with
#       BiocGenerics::rowSums()
#' @importFrom methods setMethod
setMethod(".rowSums2", "Matrix",
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

# TODO: Don't think I need this
# TODO: There's no rowSums,DataFrame-method in S4Vectors or elsewhere, so
#       can't defer to that like is done for other seed types. This
#       implementation explicitly coerces the DataFrame to an array, which
#       incurs a cost.
#       However, it's not clear that it's worth the effort to write an
#       explicit rowSums2,DataFrame-method
#' @importFrom matrixStats rowSums2
#' @importFrom methods setMethod
setMethod(".rowSums2", "DataFrame",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                   ...) {
            message("DataFrame")
            if (!is.null(rows)) {
              x <- x[rows, , drop = FALSE]
            }
            if (!is.null(cols)) {
              x <- x[, cols, drop = FALSE]
            }
            x <- as.matrix(x)
            # NOTE: Subsetting by rows and cols has already occured, so set
            #       both to NULL in following call
            matrixStats::rowSums2(x, rows = NULL, cols = NULL, na.rm, dim.)
          }
)

# TODO: Could have rowSums2,ANY-method be like rowSums2,RleArraySeed-method and
#       rowSums2,HDF5ArraySeed-method. Need to think if this is a good idea;
#       simplifies code but does it introduce overhead?
# TODO: This is the rowSums2,data.frame-method
# NOTE: I don't think there is a super efficient rowSums2,RleMatrix-method
#       because an RleMatrix is a column-wise Rle, which is inefficient to
#       iterate over row-wise
setMethod(".rowSums2", "data.frame",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                   ...) {
            message("ANY")
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

# NOTE: This feels a little circular since it takes an ANY (HDF5ArraySeed or
#       RleArraySeed, currently) as a seed, contructs an DelayedMatrix from
#       that seed, subsets the DelayedMatrix, and then calls
#       rowSums,DelayedMatrix-method. All of this is to avoid writing an
#       explicit rowSums method for these complex classes of seeds, for which
#       it is likely to be difficult to  write an implementation that is more
#       efficient than the "block processing" strategy used by
#       rowSums,DelayedMatrix-method
#' @importFrom DelayedArray DelayedArray
#' @importFrom methods setMethod
setMethod(".rowSums2", "ANY",
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
              return(rowSums(x, na.rm))

            }
            if (DelayedArray:::is_pristine(x)) {
              message("Pristine")
              x <- DelayedArray:::remove_pristine_DelayedArray_wrapping(x)
            } else {
              message("Not transposed")
              x <- .from_DelayedArray_to_simple_seed_class(x, FALSE)
            }
            .rowSums2(x, rows, cols, na.rm, dim., ...)
          }
)

# TODO: Unit tests for correctness
# TODO: Benchmarks for performance
# TODO: Some of the tricks to avoid unnecessary block processing in rowSums2()
#       are also applicable to rowSums()