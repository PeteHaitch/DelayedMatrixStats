### =============================================================================
### Non-exported
###

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

### -------------------------------------------------------------------------
### .rowSums2
###

#' @importFrom methods setGeneric
#' @inherit matrixStats::rowSums2
#' @keywords internal
setGeneric(".rowSums2", signature = "x",
           function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                    ...) standardGeneric(".rowSums2")
)

### =============================================================================
### Exported
###

### -------------------------------------------------------------------------
### rowSums2
###

#' @importFrom methods setGeneric
#' @inherit matrixStats::rowSums2
#' @export
setGeneric("rowSums2", signature = "x",
           function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                    ...) standardGeneric("rowSums2")
)


