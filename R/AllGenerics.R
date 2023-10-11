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

#' @name DelayedMatrixStats-deprecated
#' @aliases colAnyMissings DelayedMatrixStats-deprecated
#' @title Deprecated functions in package \sQuote{DelayedMatrixStats}
#' @description These functions are provided for compatibility with older
#'  versions of \sQuote{DelayedMatrixStats} only, and will be defunct at the
#'  next release.
#' @inherit MatrixGenerics::colAnyNAs
#' @template common_params
#' @template lowercase_x
#' @template useNamesParameter
#' @details The following functions are deprecated and will be made defunct;
#'  use the replacement indicated below:
#'  \itemize{
#'  \item{colAnyMissings: \code{\link{colAnyNAs}}}
#'  \item{rowAnyMissings: \code{\link{rowAnyNAs}}}
#'  }
#' @export
setGeneric("colAnyMissings", signature = "x",
           function(x, rows = NULL, cols = NULL,
                    ..., useNames = NA) {
             .Deprecated("colAnyNAs")
             standardGeneric("colAnyMissings")
           }
)

#' @rdname DelayedMatrixStats-deprecated
#' @export
setGeneric("rowAnyMissings", signature = "x",
           function(x, rows = NULL, cols = NULL,
                    ..., useNames = NA) {
             .Deprecated("rowAnyNAs")
             standardGeneric("rowAnyMissings")
           }
)
