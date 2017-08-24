### ============================================================================
### S4 generics
###

### ----------------------------------------------------------------------------
### Non-exported generics
###

# ------------------------------------------------------------------------------
# subset_simple_seed
#

# Like DelayedArray:::subset_seed_as_array except the return value has the same
# class as seed.
# NOTE: This only works for simple, in-memory seeds, e.g. matrix, Matrix,
#       and data frame. It does not work with SeedBinder or HDF5ArraySeed, for
#       example.
# TODO: Come up with a minimal working definition of a 'simple seed'
# nolint start
setGeneric("subset_simple_seed_as_seed_class", signature = "seed",
           function(seed, index) standardGeneric("subset_simple_seed_as_seed_class")
)
# nolint end

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

#' @importFrom methods setGeneric
#' @inherit matrixStats::colAlls
#' @export
setGeneric("colAlls", signature = "x",
           function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE,
                    dim. = dim(x), ...) standardGeneric("colAlls")
)

#' @importFrom methods setGeneric
#' @inherit matrixStats::colAnyMissings
#' @export
setGeneric("colAnyMissings", signature = "x",
           function(x, rows = NULL, cols = NULL,
                    ...) standardGeneric("colAnyMissings")
)

#' @importFrom methods setGeneric
#' @inherit matrixStats::colAnyNAs
#' @export
setGeneric("colAnyNAs", signature = "x",
           function(x, rows = NULL, cols = NULL,
                    ...) standardGeneric("colAnyNAs")
)

#' @importFrom methods setGeneric
#' @inherit matrixStats::colAnys
#' @export
setGeneric("colAnys", signature = "x",
           function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE,
                    dim. = dim(x), ...) standardGeneric("colAnys")
)

# # TODO: Check with Henrik why `...` appear in different place in signature for
# #       colAvgsPerRowSet and rowAvgsPerColSet
# # TODO: Check with Henrik why first arg is `X` and not `x`
# #' @importFrom methods setGeneric
# #' @inherit matrixStats::colAvgsPerRowSet
# #' @export
# setGeneric("colAvgsPerRowSet", signature = "X",
#            function(X, W = NULL, cols = NULL, S, FUN = colMeans, tFUN = FALSE,
#                     ...) standardGeneric("colAvgsPerRowSet")
# )

# #' @importFrom methods setGeneric
# #' @inherit matrixStats::colCollapse
# #' @export
# setGeneric("colCollapse", signature = "x",
#            function(x, idxs, cols = NULL, dim. = dim(x),
#                     ...) standardGeneric("colCollapse")
# )

#' @importFrom methods setGeneric
#' @inherit matrixStats::colCounts
#' @export
setGeneric("colCounts", signature = "x",
           function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE,
                    dim. = dim(x), ...) standardGeneric("colCounts")
)

# TODO: Ask Henrik why na.rm isn't an argument?
#' @importFrom methods setGeneric
#' @inherit matrixStats::colCummaxs
#' @export
setGeneric("colCummaxs", signature = "x",
           function(x, rows = NULL, cols = NULL, dim. = dim(x),
                    ...) standardGeneric("colCummaxs")
)

# TODO: Ask Henrik why na.rm isn't an argument?
#' @importFrom methods setGeneric
#' @inherit matrixStats::colCummins
#' @export
setGeneric("colCummins", signature = "x",
           function(x, rows = NULL, cols = NULL, dim. = dim(x),
                    ...) standardGeneric("colCummins")
)

#' @importFrom methods setGeneric
#' @importFrom methods setGeneric
#' @inherit matrixStats::colCumprods
#' @export
setGeneric("colCumprods", signature = "x",
           function(x, rows = NULL, cols = NULL, dim. = dim(x),
                    ...) standardGeneric("colCumprods")
)

#' @importFrom methods setGeneric
#' @importFrom methods setGeneric
#' @inherit matrixStats::colCumsums
#' @export
setGeneric("colCumsums", signature = "x",
           function(x, rows = NULL, cols = NULL, dim. = dim(x),
                    ...) standardGeneric("colCumsums")
)

#' @importFrom methods setGeneric
#' @inherit matrixStats::colDiffs
#' @export
setGeneric("colDiffs", signature = "x",
           function(x, rows = NULL, cols = NULL, lag = 1L, differences = 1L,
                    dim. = dim(x), ...) standardGeneric("colDiffs")
)

# TODO: Ask Henrik why this function keep colnames whereas most don't
#' @importFrom methods setGeneric
#' @inherit matrixStats::colIQRDiffs
#' @export
setGeneric("colIQRDiffs", signature = "x",
           function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L,
                    trim = 0, ...) standardGeneric("colIQRDiffs")
)

#' @importFrom methods setGeneric
#' @inherit matrixStats::colIQRs
#' @export
setGeneric("colIQRs", signature = "x",
           function(x, rows = NULL, cols = NULL, na.rm = FALSE,
                    ...) standardGeneric("colIQRs")
)

#' @importFrom methods setGeneric
#' @inherit matrixStats::colLogSumExps
#' @export
setGeneric("colLogSumExps", signature = "lx",
           function(lx, rows = NULL, cols = NULL, na.rm = FALSE,
                    dim. = dim(lx), ...) standardGeneric("colLogSumExps")
)

# TODO: Ask Henrik why this function keep colnames whereas most don't
#' @importFrom methods setGeneric
#' @inherit matrixStats::colMadDiffs
#' @export
setGeneric("colMadDiffs", signature = "x",
           function(x, rows = NULL, cols = NULL, na.rm = FALSE,
                    diff = 1L, trim = 0, ...) standardGeneric("colMadDiffs")
)

#' @importFrom methods setGeneric
#' @inherit matrixStats::colMads
#' @export
setGeneric("colMads", signature = "x",
           function(x, rows = NULL, cols = NULL, center = NULL,
                    constant = 1.4826, na.rm = FALSE,
                    dim. = dim(x), centers = NULL,
                    ...) standardGeneric("colMads")
)

# TODO: Need to think about interaction with methods and docs in DelayedArray
# NOTE: colMaxs S4 generic and colMaxs,DelayedMatrix-method already defined in
#       DelayedArray
# #' @inherit matrixStats::colMaxs
# #' @importMethodsFrom DelayedArray colMaxs

#' @importFrom methods setGeneric
#' @inherit matrixStats::colMeans2
#' @export
setGeneric("colMeans2", signature = "x",
           function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                    ...) standardGeneric("colMeans2")
)

#' @importFrom methods setGeneric
#' @inherit matrixStats::colMedians
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

# TODO: Ask Henrik why na.rm isn't an argument (this is noted in docs)
#' @importFrom methods setGeneric
#' @inherit matrixStats::colOrderStats
#' @export
setGeneric("colOrderStats", signature = "x",
           function(x, rows = NULL, cols = NULL, which, dim. = dim(x),
                    ...) standardGeneric("colOrderStats")
)

#' @importFrom methods setGeneric
#' @inherit matrixStats::colProds
#' @export
setGeneric("colProds", signature = "x",
           function(x, rows = NULL, cols = NULL, na.rm = FALSE,
                    method = c("direct", "expSumLog"),
                    ...) standardGeneric("colProds")
)

# TODO: Ask Henrik why matrixStats::colQuantiles() returns rownames in certain
#       circumstances but not others (e.g., 1-column matrix has not dimnames,
#       if matrix has NA/NaN)
#' @importFrom methods setGeneric
#' @inherit matrixStats::colQuantiles
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

#' @importFrom methods setGeneric
#' @inherit matrixStats::colRanks
#' @export
setGeneric("colRanks", signature = "x",
           function(x, rows = NULL, cols = NULL,
                    ties.method = c("max", "average", "min"), dim. = dim(x),
                    preserveShape = FALSE, ...) standardGeneric("colRanks")
)

#' @importFrom methods setGeneric
#' @inherit matrixStats::colSdDiffs
#' @export
setGeneric("colSdDiffs", signature = "x",
           function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L,
                    trim = 0, ...) standardGeneric("colSdDiffs")
)

#' @importFrom methods setGeneric
#' @inherit matrixStats::colSds
#' @export
setGeneric("colSds", signature = "x",
           function(x, rows = NULL, cols = NULL,
                    ...) standardGeneric("colSds")
)

#' @importFrom methods setGeneric
#' @inherit matrixStats::colSums2
#' @export
setGeneric("colSums2", signature = "x",
           function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                    ...) standardGeneric("colSums2")
)

# #' @importFrom methods setGeneric
# #' @inherit matrixStats::colTabulates
# #' @export
# setGeneric("colTabulates", signature = "x",
#            function(x, rows = NULL, cols = NULL, values = NULL,
#                     ...) standardGeneric("colTabulates")
# )

#' @importFrom methods setGeneric
#' @inherit matrixStats::colVarDiffs
#' @export
setGeneric("colVarDiffs", signature = "x",
           function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L,
                    trim = 0, ...) standardGeneric("colVarDiffs")
)

#' @importFrom methods setGeneric
#' @inherit matrixStats::colVars
#' @export
setGeneric("colVars", signature = "x",
           function(x, rows = NULL, cols = NULL, na.rm = FALSE, center = NULL,
                    dim. = dim(x), ...) standardGeneric("colVars")
)

#' @importFrom methods setGeneric
#' @inherit matrixStats::colWeightedMads
#' @export
setGeneric("colWeightedMads", signature = "x",
           function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE,
                    constant = 1.4826, center = NULL,
                    ...) standardGeneric("colWeightedMads")
)

#' @importFrom methods setGeneric
#' @inherit matrixStats::colWeightedMeans
#' @export
setGeneric("colWeightedMeans", signature = "x",
           function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE,
                    ...) standardGeneric("colWeightedMeans")
)

#' @importFrom methods setGeneric
#' @inherit matrixStats::colWeightedMedians
#' @export
setGeneric("colWeightedMedians", signature = "x",
           function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE,
                    ...) standardGeneric("colWeightedMedians")
)

#' @importFrom methods setGeneric
#' @inherit matrixStats::colWeightedSds
#' @export
setGeneric("colWeightedSds", signature = "x",
           function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE,
                    ...) standardGeneric("colWeightedSds")
)

#' @importFrom methods setGeneric
#' @inherit matrixStats::colWeightedVars
#' @export
setGeneric("colWeightedVars", signature = "x",
           function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE,
                    ...) standardGeneric("colWeightedVars")
)

# ------------------------------------------------------------------------------
# rowFoos
#

#' @importFrom methods setGeneric
#' @rdname colAlls
#' @export
setGeneric("rowAlls", signature = "x",
           function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE,
                    dim. = dim(x), ...) standardGeneric("rowAlls")
)

#' @importFrom methods setGeneric
#' @rdname colAnyMissings
#' @export
setGeneric("rowAnyMissings", signature = "x",
           function(x, rows = NULL, cols = NULL,
                    ...) standardGeneric("rowAnyMissings")
)

#' @importFrom methods setGeneric
#' @rdname colAnyNAs
#' @export
setGeneric("rowAnyNAs", signature = "x",
           function(x, rows = NULL, cols = NULL,
                    ...) standardGeneric("rowAnyNAs")
)

#' @importFrom methods setGeneric
#' @rdname colAnys
#' @export
setGeneric("rowAnys", signature = "x",
           function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE,
                    dim. = dim(x), ...) standardGeneric("rowAnys")
)

# # TODO: Check with Henrik why `...` appear in different place in signature for
# #       colAvgsPerRowSet and rowAvgsPerColSet
# # TODO: Check with Henrik why first arg is `X` and not `x`
# #' @importFrom methods setGeneric
# #' @rdname colAvgsPerRowSet
# #' @export
# setGeneric("rowAvgsPerRowSet", signature = "X",
#            function(X, W = NULL, cols = NULL, S, FUN = colMeans, tFUN = FALSE,
#                     ...) standardGeneric("rowAvgsPerRowSet")
# )

# #' @importFrom methods setGeneric
# #' @rdname colCollapse
# #' @export
# setGeneric("rowCollapse", signature = "x",
#            function(x, idxs, cols = NULL, dim. = dim(x),
#                     ...) standardGeneric("rowCollapse")
# )

#' @importFrom methods setGeneric
#' @rdname colCounts
#' @export
setGeneric("rowCounts", signature = "x",
           function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE,
                    dim. = dim(x), ...) standardGeneric("rowCounts")
)

# TODO: Ask Henrik why na.rm isn't an argument?
#' @importFrom methods setGeneric
#' @rdname colCummaxs
#' @export
setGeneric("rowCummaxs", signature = "x",
           function(x, rows = NULL, cols = NULL, dim. = dim(x),
                    ...) standardGeneric("rowCummaxs")
)

# TODO: Ask Henrik why na.rm isn't an argument?
#' @importFrom methods setGeneric
#' @rdname colCummins
#' @export
setGeneric("rowCummins", signature = "x",
           function(x, rows = NULL, cols = NULL, dim. = dim(x),
                    ...) standardGeneric("rowCummins")
)

#' @importFrom methods setGeneric
#' @rdname colCumprods
#' @export
setGeneric("rowCumprods", signature = "x",
           function(x, rows = NULL, cols = NULL, dim. = dim(x),
                    ...) standardGeneric("rowCumprods")
)

#' @importFrom methods setGeneric
#' @rdname colCumsums
#' @export
setGeneric("rowCumsums", signature = "x",
           function(x, rows = NULL, cols = NULL, dim. = dim(x),
                    ...) standardGeneric("rowCumsums")
)

#' @importFrom methods setGeneric
#' @rdname colDiffs
#' @export
setGeneric("rowDiffs", signature = "x",
           function(x, rows = NULL, cols = NULL, lag = 1L, differences = 1L,
                    dim. = dim(x), ...) standardGeneric("rowDiffs")
)

# TODO: Ask Henrik why this function keep colnames whereas most don't
#' @importFrom methods setGeneric
#' @rdname colIQRDiffs
#' @export
setGeneric("rowIQRDiffs", signature = "x",
           function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L,
                    trim = 0, ...) standardGeneric("rowIQRDiffs")
)

#' @importFrom methods setGeneric
#' @rdname colIQRs
#' @export
setGeneric("rowIQRs", signature = "x",
           function(x, rows = NULL, cols = NULL, na.rm = FALSE,
                    ...) standardGeneric("rowIQRs")
)

#' @importFrom methods setGeneric
#' @rdname colLogSumExps
#' @export
setGeneric("rowLogSumExps", signature = "lx",
           function(lx, rows = NULL, cols = NULL, na.rm = FALSE,
                    dim. = dim(lx), ...) standardGeneric("rowLogSumExps")
)

# TODO: Ask Henrik why this function keep colnames whereas most don't
#' @importFrom methods setGeneric
#' @rdname colMadDiffs
#' @export
setGeneric("rowMadDiffs", signature = "x",
           function(x, rows = NULL, cols = NULL, na.rm = FALSE,
                    diff = 1L, trim = 0, ...) standardGeneric("rowMadDiffs")
)

#' @importFrom methods setGeneric
#' @rdname colMads
#' @export
setGeneric("rowMads", signature = "x",
           function(x, rows = NULL, cols = NULL, center = NULL,
                    constant = 1.4826, na.rm = FALSE,
                    dim. = dim(x), centers = NULL,
                    ...) standardGeneric("rowMads")
)

# TODO: Need to think about interaction with methods and docs in DelayedArray
# NOTE: rowMaxs S4 generic and rowMaxs,DelayedMatrix-method already defined in
#       DelayedArray
# #' @rdname colMaxs
# #' @importMethodsFrom DelayedArray rowMaxs

#' @importFrom methods setGeneric
#' @rdname colMeans2
#' @export
setGeneric("rowMeans2", signature = "x",
           function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                    ...) standardGeneric("rowMeans2")
)

#' @importFrom methods setGeneric
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

# TODO: Ask Henrik why na.rm isn't an argument (this is noted in docs)
#' @importFrom methods setGeneric
#' @rdname colOrderStats
#' @export
setGeneric("rowOrderStats", signature = "x",
           function(x, rows = NULL, cols = NULL, which, dim. = dim(x),
                    ...) standardGeneric("rowOrderStats")
)

#' @importFrom methods setGeneric
#' @rdname colProds
#' @export
setGeneric("rowProds", signature = "x",
           function(x, rows = NULL, cols = NULL, na.rm = FALSE,
                    method = c("direct", "expSumLog"),
                    ...) standardGeneric("rowProds")
)

# TODO: Ask Henrik why matrixStats::rowQuantiles() returns rownames in certain
#       circumstances but not others (e.g., 1-column matrix has not dimnames,
#       if matrix has NA/NaN)
#' @importFrom methods setGeneric
#' @rdname colQuantiles
#' @export
setGeneric("rowQuantiles", signature = "x",
           function(x, rows = NULL, cols = NULL,
                    probs = seq(from = 0, to = 1, by = 0.25), na.rm = FALSE,
                    type = 7L, ..., drop = TRUE) standardGeneric("rowQuantiles")
)
