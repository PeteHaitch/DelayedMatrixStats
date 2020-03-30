### ============================================================================
### S4 generics
###

### ----------------------------------------------------------------------------
### Non-exported generics
###

# ------------------------------------------------------------------------------
# subset_by_Nindex()
#

#' `subset_by_Nindex`
#'
#' `subset_by_Nindex()` is an internal generic function not aimed to be used
#' directly by the user. It is basically an S4 generic for
#' `DelayedArray:::subset_by_Nindex`.
#'
#' @param x An array-like object.
#' @param Nindex An unnamed list of subscripts as positive integer vectors, one
#' vector per dimension in `x`. Empty and missing subscripts (represented by
#' `integer(0)` and `NULL` list elements, respectively) are allowed. The
#' subscripts can contain duplicated indices. They cannot contain `NA`s or
#' non-positive values.
#'
#' @details
#' `subset_by_Nindex(x, Nindex)` conceptually performs the operation
#' \code{x[Nindex[1], ..., Nindex[length(Nindex)])}. `subset_by_Nindex()`
#' methods need to support empty and missing subscripts, e.g.,
#' `subset_by_Nindex(x, list(NULL, integer(0)))` must return an M x 0 object
#' of class `class(x)` and `subset_by_Nindex(x, list(integer(0), integer(0)))`
#' a 0 x 0 object of class `class(x)`.
#'
#' Also, subscripts are allowed to contain duplicate indices so things like
#' `subset_by_Nindex(x, list(c(1:3, 3:1), 2L))` need to be supported.
#'
#' @return A object of class `class(x)` of the appropriate type (e.g., integer,
#' double, etc.). For example, if `x` is a [data.frame] representing an M x N
#' matrix of integers, `subset_by_Nindex(x, list(NULL, 2L)` must return its 2nd
#' column as a [data.frame] with M rows and 1 column of type integer.
#'
setGeneric("subset_by_Nindex", signature = "x",
           function(x, Nindex) standardGeneric("subset_by_Nindex")
)

### ----------------------------------------------------------------------------
### Exported generics
###

# NOTE: For a function, matrixStats::foo(), we must not create the S4 generic
#       via `setGeneric("foo")` or `setGeneric("foo", signature = "x')`
#       [i.e do not leave `def` empty in the call to setGeneric()] nor can we
#       import matrixStats::foo() into the NAMESPACE of this package
#       [i.e. must always use matrixStats::foo() in package code]. In both
#       cases, what otherwise happens is that a default foo,ANY-method is
#       created, which causes all sorts of inheritance headaches. Instead, by
#       explicitly defining `def` in the call to setGeneric("foo"), we ensure
#       that the foo,ANY-method is not created. We can still achieve the
#       desired default behaviour by
#       `setMethod("foo", "matrix", matrixStats::foo)`.
#       Obviously, the formals of `def` should match the `formals(foo)` to
#       provide a consistent/familiar API to the user

# ------------------------------------------------------------------------------
# colFoos
#

#' Deprecated functions in \sQuote{DelayedMatrixStats}
#'
#' These functions are provided for compatibility with older versions of
#' \sQuote{DelayedMatrixStats} only, and will be defunct at the next release.
#'
#' @details The following functions are deprecated and will be made defunct;
#'   use the replacement indicated below:
#'   \itemize{
#'     \item{\code{colAnyMissings()}: \code{\link{colAnyNAs}()}}
#'     \item{\code{rowAnyMissings()}: \code{\link{rowAnyNAs}()}}
#'   }
#' @inheritParams MatrixGenerics::colAnyNAs
#' @template lowercase_x
#' @rdname DelayedMatrixStats-deprecated
#' @export
colAnyMissings <- function(x, rows = NULL, cols = NULL, ...) {
  .Deprecated("colAnyNAs")
  colAnyNAs(x, rows = rows, cols = cols, ...)
}

# ------------------------------------------------------------------------------
# rowFoos
#

#' @rdname DelayedMatrixStats-deprecated
#' @inheritParams MatrixGenerics::rowAnyNAs
#' @template lowercase_x
#' @export
rowAnyMissings <- function(x, rows = NULL, cols = NULL, ...) {
  .Deprecated("rowAnyNAs")
  rowAnyNAs(x, rows = rows, cols = cols, ...)
}