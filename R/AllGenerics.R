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

#' @inherit matrixStats::colAlls
#' @rdname colAlls
#' @export
setGeneric("colAlls", signature = "x",
           function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE,
                    dim. = dim(x), ...) standardGeneric("colAlls")
)

#' @inherit matrixStats::colAnyMissings
#' @rdname colAnyNAs
#' @export
setGeneric("colAnyMissings", signature = "x",
           function(x, rows = NULL, cols = NULL,
                    ...) standardGeneric("colAnyMissings")
)

#' @inherit matrixStats::colAnyNAs
#' @rdname colAnyNAs
#' @export
setGeneric("colAnyNAs", signature = "x",
           function(x, rows = NULL, cols = NULL,
                    ...) standardGeneric("colAnyNAs")
)

#' @inherit matrixStats::colAnys
#' @rdname colAlls
#' @export
setGeneric("colAnys", signature = "x",
           function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE,
                    dim. = dim(x), ...) standardGeneric("colAnys")
)

#' @inherit matrixStats::colAvgsPerRowSet
#' @rdname colAvgsPerRowSet
#' @export
setGeneric("colAvgsPerRowSet", signature = "X",
           function(X, W = NULL, cols = NULL, S, FUN = colMeans, ...,
                    tFUN = FALSE) standardGeneric("colAvgsPerRowSet")
)

#' @inherit matrixStats::colCollapse
#' @rdname colCollapse
#' @export
setGeneric("colCollapse", signature = "x",
           function(x, idxs, cols = NULL, dim. = dim(x),
                    ...) standardGeneric("colCollapse")
)

#' @inherit matrixStats::colCounts
#' @rdname colCounts
#' @export
setGeneric("colCounts", signature = "x",
           function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE,
                    dim. = dim(x), ...) standardGeneric("colCounts")
)

#' @inherit matrixStats::colCummaxs
#' @rdname colCummaxs
#' @export
setGeneric("colCummaxs", signature = "x",
           function(x, rows = NULL, cols = NULL, dim. = dim(x),
                    ...) standardGeneric("colCummaxs")
)

#' @inherit matrixStats::colCummins
#' @rdname colCummaxs
#' @export
setGeneric("colCummins", signature = "x",
           function(x, rows = NULL, cols = NULL, dim. = dim(x),
                    ...) standardGeneric("colCummins")
)

#' @inherit matrixStats::colCumprods
#' @rdname colCummaxs
#' @export
setGeneric("colCumprods", signature = "x",
           function(x, rows = NULL, cols = NULL, dim. = dim(x),
                    ...) standardGeneric("colCumprods")
)

#' @inherit matrixStats::colCumsums
#' @rdname colCummaxs
#' @export
setGeneric("colCumsums", signature = "x",
           function(x, rows = NULL, cols = NULL, dim. = dim(x),
                    ...) standardGeneric("colCumsums")
)

#' @inherit matrixStats::colDiffs
#' @rdname colDiffs
#' @export
setGeneric("colDiffs", signature = "x",
           function(x, rows = NULL, cols = NULL, lag = 1L, differences = 1L,
                    dim. = dim(x), ...) standardGeneric("colDiffs")
)

#' @inherit matrixStats::colIQRDiffs
#' @rdname colIQRDiffs
#' @export
setGeneric("colIQRDiffs", signature = "x",
           function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L,
                    trim = 0, ...) standardGeneric("colIQRDiffs")
)

#' @inherit matrixStats::colIQRs
#' @rdname colIQRs
#' @export
setGeneric("colIQRs", signature = "x",
           function(x, rows = NULL, cols = NULL, na.rm = FALSE,
                    ...) standardGeneric("colIQRs")
)

#' @inherit matrixStats::colLogSumExps
#' @rdname colLogSumExps
#' @export
setGeneric("colLogSumExps", signature = "lx",
           function(lx, rows = NULL, cols = NULL, na.rm = FALSE,
                    dim. = dim(lx), ...) standardGeneric("colLogSumExps")
)

#' @inherit matrixStats::colMadDiffs
#' @rdname colIQRDiffs
#' @export
setGeneric("colMadDiffs", signature = "x",
           function(x, rows = NULL, cols = NULL, na.rm = FALSE,
                    diff = 1L, trim = 0, ...) standardGeneric("colMadDiffs")
)

#' @inherit matrixStats::colMads
#' @rdname colMads
#' @export
setGeneric("colMads", signature = "x",
           function(x, rows = NULL, cols = NULL, center = NULL,
                    constant = 1.4826, na.rm = FALSE,
                    dim. = dim(x), ...) standardGeneric("colMads")
)

# TODO: Need to think about interaction with methods and docs in DelayedArray
# NOTE: colMaxs S4 generic and colMaxs,DelayedMatrix-method already defined in
#       DelayedArray
# #' @inherit matrixStats::colMaxs
# #' @importMethodsFrom DelayedArray colMaxs

#' @inherit matrixStats::colMeans2
#' @rdname colMeans2
#' @export
setGeneric("colMeans2", signature = "x",
           function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                    ...) standardGeneric("colMeans2")
)

#' @inherit matrixStats::colMedians
#' @rdname colMedians
#' @export
setGeneric("colMedians", signature = "x",
           function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                    ...) standardGeneric("colMedians")
)

# TODO: Need to think about interaction with methods and docs in DelayedArray
# NOTE: colMins S4 generic and colMins,DelayedMatrix-method, already defined in
#       DelayedArray
# #' @inherit matrixStats::colMins
# #' @importMethodsFrom DelayedArray colMins

#' @inherit matrixStats::colOrderStats
#' @rdname colOrderStats
#' @export
setGeneric("colOrderStats", signature = "x",
           function(x, rows = NULL, cols = NULL, which, dim. = dim(x),
                    ...) standardGeneric("colOrderStats")
)

#' @inherit matrixStats::colProds
#' @rdname colProds
#' @export
setGeneric("colProds", signature = "x",
           function(x, rows = NULL, cols = NULL, na.rm = FALSE,
                    method = c("direct", "expSumLog"),
                    ...) standardGeneric("colProds")
)

#' @inherit matrixStats::colQuantiles
#' @rdname colQuantiles
#' @export
setGeneric("colQuantiles", signature = "x",
           function(x, rows = NULL, cols = NULL,
                    probs = seq(from = 0, to = 1, by = 0.25), na.rm = FALSE,
                    type = 7L, ..., drop = TRUE) standardGeneric("colQuantiles")
)

# TODO: Need to think about interaction with methods and docs in DelayedArray
# NOTE: colRanges S4 generic and colRanges,DelayedMatrix-method already defined
#       in DelayedArray
# #' @inherit matrixStats::colRanges
# #' @importMethodsFrom DelayedArray colRanges

#' @inherit matrixStats::colRanks
#' @rdname colRanks
#' @export
setGeneric("colRanks", signature = "x",
           function(x, rows = NULL, cols = NULL,
                    ties.method = c("max", "average", "min"), dim. = dim(x),
                    preserveShape = FALSE, ...) standardGeneric("colRanks")
)

#' @inherit matrixStats::colSdDiffs
#' @rdname colIQRDiffs
#' @export
setGeneric("colSdDiffs", signature = "x",
           function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L,
                    trim = 0, ...) standardGeneric("colSdDiffs")
)

#' @inherit matrixStats::colSds
#' @rdname colMads
#' @export
setGeneric("colSds", signature = "x",
           function(x, rows = NULL, cols = NULL, ...) standardGeneric("colSds")
)

#' @inherit matrixStats::colSums2
#' @rdname colSums2
#' @export
setGeneric("colSums2", signature = "x",
           function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                    ...) standardGeneric("colSums2")
)

#' @inherit matrixStats::colTabulates
#' @rdname colTabulates
#' @export
setGeneric("colTabulates", signature = "x",
           function(x, rows = NULL, cols = NULL, values = NULL,
                    ...) standardGeneric("colTabulates")
)

#' @inherit matrixStats::colVarDiffs
#' @rdname colIQRDiffs
#' @export
setGeneric("colVarDiffs", signature = "x",
           function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L,
                    trim = 0, ...) standardGeneric("colVarDiffs")
)

#' @inherit matrixStats::colVars
#' @rdname colVars
#' @export
setGeneric("colVars", signature = "x",
           function(x, rows = NULL, cols = NULL, na.rm = FALSE, center = NULL,
                    dim. = dim(x), ...) standardGeneric("colVars")
)

#' @inherit matrixStats::colWeightedMads
#' @rdname colWeightedMads
#' @export
setGeneric("colWeightedMads", signature = "x",
           function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE,
                    constant = 1.4826, center = NULL,
                    ...) standardGeneric("colWeightedMads")
)

#' @inherit matrixStats::colWeightedMeans
#' @rdname colWeightedMeans
#' @export
setGeneric("colWeightedMeans", signature = "x",
           function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE,
                    ...) standardGeneric("colWeightedMeans")
)

#' @inherit matrixStats::colWeightedMedians
#' @rdname colWeightedMedians
#' @export
setGeneric("colWeightedMedians", signature = "x",
           function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE,
                    ...) standardGeneric("colWeightedMedians")
)

#' @inherit matrixStats::colWeightedVars
#' @rdname colWeightedVars
#' @export
setGeneric("colWeightedSds", signature = "x",
           function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE,
                    ...) standardGeneric("colWeightedSds")
)

#' @inherit matrixStats::colWeightedVars
#' @rdname colWeightedVars
#' @export
setGeneric("colWeightedVars", signature = "x",
           function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE,
                    ...) standardGeneric("colWeightedVars")
)

# ------------------------------------------------------------------------------
# rowFoos
#

#' @rdname colAlls
#' @export
setGeneric("rowAlls", signature = "x",
           function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE,
                    dim. = dim(x), ...) standardGeneric("rowAlls")
)

#' @rdname colAnyNAs
#' @export
setGeneric("rowAnyMissings", signature = "x",
           function(x, rows = NULL, cols = NULL,
                    ...) standardGeneric("rowAnyMissings")
)

#' @rdname colAnyNAs
#' @export
setGeneric("rowAnyNAs", signature = "x",
           function(x, rows = NULL, cols = NULL,
                    ...) standardGeneric("rowAnyNAs")
)

#' @rdname colAlls
#' @export
setGeneric("rowAnys", signature = "x",
           function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE,
                    dim. = dim(x), ...) standardGeneric("rowAnys")
)

#' @rdname colAvgsPerRowSet
#' @export
setGeneric("rowAvgsPerColSet", signature = "X",
           function(X, W = NULL, rows = NULL, S, FUN = rowMeans, ...,
                    tFUN = FALSE) standardGeneric("rowAvgsPerColSet")
)

#' @rdname colCollapse
#' @export
setGeneric("rowCollapse", signature = "x",
           function(x, idxs, rows = NULL, dim. = dim(x),
                    ...) standardGeneric("rowCollapse")
)

#' @rdname colCounts
#' @export
setGeneric("rowCounts", signature = "x",
           function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE,
                    dim. = dim(x), ...) standardGeneric("rowCounts")
)

#' @rdname colCummaxs
#' @export
setGeneric("rowCummaxs", signature = "x",
           function(x, rows = NULL, cols = NULL, dim. = dim(x),
                    ...) standardGeneric("rowCummaxs")
)

#' @rdname colCummaxs
#' @export
setGeneric("rowCummins", signature = "x",
           function(x, rows = NULL, cols = NULL, dim. = dim(x),
                    ...) standardGeneric("rowCummins")
)

#' @rdname colCummaxs
#' @export
setGeneric("rowCumprods", signature = "x",
           function(x, rows = NULL, cols = NULL, dim. = dim(x),
                    ...) standardGeneric("rowCumprods")
)

#' @rdname colCummaxs
#' @export
setGeneric("rowCumsums", signature = "x",
           function(x, rows = NULL, cols = NULL, dim. = dim(x),
                    ...) standardGeneric("rowCumsums")
)

#' @rdname colDiffs
#' @export
setGeneric("rowDiffs", signature = "x",
           function(x, rows = NULL, cols = NULL, lag = 1L, differences = 1L,
                    dim. = dim(x), ...) standardGeneric("rowDiffs")
)

#' @rdname colIQRDiffs
#' @export
setGeneric("rowIQRDiffs", signature = "x",
           function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L,
                    trim = 0, ...) standardGeneric("rowIQRDiffs")
)

#' @rdname colIQRs
#' @export
setGeneric("rowIQRs", signature = "x",
           function(x, rows = NULL, cols = NULL, na.rm = FALSE,
                    ...) standardGeneric("rowIQRs")
)

#' @rdname colLogSumExps
#' @export
setGeneric("rowLogSumExps", signature = "lx",
           function(lx, rows = NULL, cols = NULL, na.rm = FALSE,
                    dim. = dim(lx), ...) standardGeneric("rowLogSumExps")
)

#' @rdname colIQRDiffs
#' @export
setGeneric("rowMadDiffs", signature = "x",
           function(x, rows = NULL, cols = NULL, na.rm = FALSE,
                    diff = 1L, trim = 0, ...) standardGeneric("rowMadDiffs")
)

#' @rdname colMads
#' @export
setGeneric("rowMads", signature = "x",
           function(x, rows = NULL, cols = NULL, center = NULL,
                    constant = 1.4826, na.rm = FALSE,
                    dim. = dim(x), ...) standardGeneric("rowMads")
)

# TODO: Need to think about interaction with methods and docs in DelayedArray
# NOTE: rowMaxs S4 generic and rowMaxs,DelayedMatrix-method already defined in
#       DelayedArray
# #' @rdname colMaxs
# #' @importMethodsFrom DelayedArray rowMaxs

#' @rdname colMeans2
#' @export
setGeneric("rowMeans2", signature = "x",
           function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                    ...) standardGeneric("rowMeans2")
)

#' @rdname colMedians
#' @export
setGeneric("rowMedians", signature = "x",
           function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                    ...) standardGeneric("rowMedians")
)

# TODO: Need to think about interaction with methods and docs in DelayedArray
# NOTE: rowMins S4 generic and rowMins,DelayedMatrix-method, already defined in
#       DelayedArray
# #' @rdname colMins
# #' @importMethodsFrom DelayedArray rowMins

#' @rdname colOrderStats
#' @export
setGeneric("rowOrderStats", signature = "x",
           function(x, rows = NULL, cols = NULL, which, dim. = dim(x),
                    ...) standardGeneric("rowOrderStats")
)

#' @rdname colProds
#' @export
setGeneric("rowProds", signature = "x",
           function(x, rows = NULL, cols = NULL, na.rm = FALSE,
                    method = c("direct", "expSumLog"),
                    ...) standardGeneric("rowProds")
)

#' @rdname colQuantiles
#' @export
setGeneric("rowQuantiles", signature = "x",
           function(x, rows = NULL, cols = NULL,
                    probs = seq(from = 0, to = 1, by = 0.25), na.rm = FALSE,
                    type = 7L, ..., drop = TRUE) standardGeneric("rowQuantiles")
)

# TODO: Need to think about interaction with methods and docs in DelayedArray
# NOTE: rowRanges S4 generic and rowRanges,DelayedMatrix-method already defined
#       in DelayedArray
# #' @rdname colRanges
# #' @importMethodsFrom DelayedArray rowRanges

# TODO: Ask Henrik why rowRanks() does not have the preserveShape argument
#' @rdname colRanks
#' @export
setGeneric("rowRanks", signature = "x",
           function(x, rows = NULL, cols = NULL,
                    ties.method = c("max", "average", "min"), dim. = dim(x),
                    ...) standardGeneric("rowRanks")
)

#' @rdname colIQRDiffs
#' @export
setGeneric("rowSdDiffs", signature = "x",
           function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L,
                    trim = 0, ...) standardGeneric("rowSdDiffs")
)

#' @rdname colMads
#' @export
setGeneric("rowSds", signature = "x",
           function(x, rows = NULL, cols = NULL, ...) standardGeneric("rowSds")
)

#' @rdname colSums2
#' @export
setGeneric("rowSums2", signature = "x",
           function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                    ...) standardGeneric("rowSums2")
)

#' @rdname colTabulates
#' @export
setGeneric("rowTabulates", signature = "x",
           function(x, rows = NULL, cols = NULL, values = NULL,
                    ...) standardGeneric("rowTabulates")
)

#' @rdname colIQRDiffs
#' @export
setGeneric("rowVarDiffs", signature = "x",
           function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L,
                    trim = 0, ...) standardGeneric("rowVarDiffs")
)

#' @rdname colVars
#' @export
setGeneric("rowVars", signature = "x",
           function(x, rows = NULL, cols = NULL, na.rm = FALSE, center = NULL,
                    dim. = dim(x), ...) standardGeneric("rowVars")
)

#' @rdname colWeightedMads
#' @export
setGeneric("rowWeightedMads", signature = "x",
           function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE,
                    constant = 1.4826, center = NULL,
                    ...) standardGeneric("rowWeightedMads")
)

#' @rdname colWeightedMeans
#' @export
setGeneric("rowWeightedMeans", signature = "x",
           function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE,
                    ...) standardGeneric("rowWeightedMeans")
)

#' @rdname colWeightedMedians
#' @export
setGeneric("rowWeightedMedians", signature = "x",
           function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE,
                    ...) standardGeneric("rowWeightedMedians")
)

#' @rdname colWeightedVars
#' @export
setGeneric("rowWeightedSds", signature = "x",
           function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE,
                    ...) standardGeneric("rowWeightedSds")
)

#' @rdname colWeightedVars
#' @export
setGeneric("rowWeightedVars", signature = "x",
           function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE,
                    ...) standardGeneric("rowWeightedVars")
)
