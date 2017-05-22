### =============================================================================
### Non-exported functions
###

### -------------------------------------------------------------------------
### .is_simple_seed
###

# TODO: Figure out a minimal definition of a simple seed
# TODO: Is an RleArraySeed a simple seed? It's in memory, but doesn't support
#       basic operations like "[", although it does support
#       subset_simple_seed_as_seed_class() and
#       DelayedArray:::subset_seed_as_array(), which may be sufficient
.is_simple_seed <- function(seed) {
  simple_seed_classes <- c("matrix", "Matrix", "data.frame", "DataFrame")
  any(vapply(simple_seed_classes, function(class) is(seed, class), logical(1)))
}

### -------------------------------------------------------------------------
### .from_DelayedArray_to_simple_seed_class
###

# UP TO HERE: Midway through writing this
# Like DelayedArray:::.from_DelayedArray_to_array but returning an object of
# the same class as class(seed(x))
# NOTE: Only works for simple, in-memory seeds
# TODO: Doesn't yet work when x is a RleArray instance; can execute delayed ops
#       on a RleArray but not on an RleArraySeed
#' @importFrom S4Vectors isTRUEorFALSE
.from_DelayedArray_to_simple_seed_class <- function(x, drop = FALSE) {
  if (!.is_simple_seed(seed(x))) {
    stop("x does not have a simple seed")
  }
  if (!isTRUEorFALSE(drop)) {
    stop("'drop' must be TRUE or FALSE")
  }
  ans <- subset_simple_seed_as_seed_class(seed(x), unname(x@index))
  # TODO: Doesn't work for data frame seed or RleArraySeed
  if (!is.data.frame(ans) && !is(ans, "RleArraySeed")) {
    dim(ans) <- DelayedArray:::.get_DelayedArray_dim_before_transpose(x)
  }
  ans <- DelayedArray:::.execute_delayed_ops(ans, x@delayed_ops)
  dimnames(ans) <- DelayedArray:::.get_DelayedArray_dimnames_before_transpose(x)
  if (drop) {
    ans <- DelayedArray:::.reduce_array_dimensions(ans)
  }
  ## Base R doesn't support transposition of an array of arbitrary dimension
  ## (generalized transposition) so the call to t() below will fail if 'ans'
  ## has more than 2 dimensions. If we want as.array() to work on a
  ## transposed DelayedArray object of arbitrary dimension, we need to
  ## implement our own generalized transposition of an ordinary array.
  if (x@is_transposed) {
    if (length(dim(ans)) > 2L) {
      stop("can't do as.array() on this object, sorry")
    }
    ans <- t(ans)
  }
  ans
}

### =============================================================================
### Non-exported methods
###

### -------------------------------------------------------------------------
### subset_simple_seed_as_seed_class
###

setMethod("subset_simple_seed_as_seed_class", "matrix",
          function(seed, index) {
            DelayedArray:::subset_by_Nindex(seed, index)
          }
)

setMethod("subset_simple_seed_as_seed_class", "Matrix",
          function(seed, index) {
            DelayedArray:::subset_by_Nindex(seed, index)
          }
)

setMethod("subset_simple_seed_as_seed_class", "data.frame",
          function(seed, index) {
            DelayedArray:::subset_by_Nindex(seed, index)
          }
)

setMethod("subset_simple_seed_as_seed_class", "DataFrame",
          function(seed, index) {
            DelayedArray:::subset_by_Nindex(seed, index)
          }
)

# TODO: Might be able to simplify to DelayedArray:::subset_by_Nindex() if
#       `[`,RleArraySeed-method is defined, e.g., via
#       DelayedArray:::to_linear_index() like in the below
setMethod("subset_simple_seed_as_seed_class", "RleArraySeed",
          function(seed, index) {
            seed_dim <- dim(seed)
            i <- DelayedArray:::to_linear_index(index, seed_dim)
            ans <- seed@rle[i]
            # TODO: What about dimnames?
            DelayedArray:::RleArraySeed(ans, lengths(index))
          }
)
