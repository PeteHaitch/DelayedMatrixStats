### ============================================================================
### S4 generics
###


# ------------------------------------------------------------------------------
# Non-exported generics
#

### -------------------------------------------------------------------------
### subset_simple_seed
###

# Like DelayedArray:::subset_seed_as_array except the return value has the same
# class as seed.
# NOTE: This only works for simple, in-memory seeds, e.g. matrix, Matrix,
#       and data frame. It does not work with SeedBinder or HDF5ArraySeed, for
#       example.
# TODO: Come up with a minimal working definition of a 'simple seed'
setGeneric("subset_simple_seed_as_seed_class", signature = "seed",
           function(seed, index) standardGeneric("subset_simple_seed_as_seed_class")
)

# ------------------------------------------------------------------------------
# Exported generics
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

# TODO: Check with Henrik why `...` appear in different place in signature for
#       colAvgsPerRowSet and rowAvgsPerColSet
# TODO: Check with Henrik why first arg is `X` and not `x`
#' @importFrom methods setGeneric
#' @inherit matrixStats::colAvgsPerRowSet
#' @export
setGeneric("colAvgsPerRowSet", signature = "X",
           function(X, W = NULL, cols = NULL, S, FUN = colMeans, tFUN = FALSE,
                    ...) standardGeneric("colAvgsPerRowSet")
)

#' @importFrom methods setGeneric
#' @inherit matrixStats::colCollapse
#' @export
setGeneric("colCollapse", signature = "x",
           function(x, idxs, cols = NULL, dim. = dim(x),
                    ...) standardGeneric("colCollapse")
)

#' @importFrom methods setGeneric
#' @inherit matrixStats::colMedians
#' @export
setGeneric("colMedians", signature = "x",
           function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                    ...) standardGeneric("colMedians")
)

#' @importFrom methods setGeneric
#' @inherit matrixStats::colSums2
#' @export
setGeneric("colSums2", signature = "x",
           function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                    ...) standardGeneric("colSums2")
)

#' @importFrom methods setGeneric
#' @inherit matrixStats::rowMedians
#' @export
setGeneric("rowMedians", signature = "x",
           function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                    ...) standardGeneric("rowMedians")
)

#' @importFrom methods setGeneric
#' @inherit matrixStats::rowSums2
#' @export
setGeneric("rowSums2", signature = "x",
           function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                    ...) standardGeneric("rowSums2")
)
