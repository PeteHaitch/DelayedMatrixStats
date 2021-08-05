### ============================================================================
### colProds
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_colProds <- function(x, rows = NULL, cols = NULL,
                                          na.rm = FALSE,
                                          method = c("direct", "expSumLog"),
                                          ..., useNames = NA) {
  # Check input
  method <- match.arg(method)
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = FALSE)

  # Subset
  x <- ..subset(x, rows, cols)

  # Compute result
  val <- colblock_APPLY(x = x,
                        FUN = colProds,
                        na.rm = na.rm,
                        method = method,
                        ...,
                        useNames = useNames)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  # NOTE: Return value of matrixStats::colProds() has no names
  # TODO: Obey top-level `useNames` argument.
  unlist(val, recursive = FALSE, use.names = FALSE)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit MatrixGenerics::colProds
#' @importMethodsFrom DelayedArray seed
#' @rdname colProds
#' @template common_params
#' @template lowercase_x
#' @template useNamesParameter
#' @author Peter Hickey
#' @export
setMethod("colProds", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE,
                   method = c("direct", "expSumLog"),
                   force_block_processing = FALSE, ..., useNames = NA) {
            method <- match.arg(method)
            .smart_seed_dispatcher(x, generic = MatrixGenerics::colProds,
                                   blockfun = .DelayedMatrix_block_colProds,
                                   force_block_processing = force_block_processing,
                                   rows = rows,
                                   cols = cols,
                                   na.rm = na.rm,
                                   #method = method,  # Wait for fix on SMS's side.
                                   ...,
                                   useNames = useNames)
          }
)

# ------------------------------------------------------------------------------
# Seed-aware methods
#

#' @importMethodsFrom IRanges Views viewApply
#' @rdname colProds
#' @export
#' @template example_dm_matrix
#' @template example_dm_HDF5
#' @examples
#'
#' colProds(dm_matrix)
setMethod("colProds", "SolidRleArraySeed",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE,
                   method = c("direct", "expSumLog"), ..., useNames = NA) {
            method <- match.arg(method)
            if (method != "direct") {
              stop("Only the 'direct' method is currently supported for ",
                   "DelayedMatrix with '", class(x), "' seed.")
            }
            message2(class(x), get_verbose())
            irl <- get_Nindex_as_IRangesList(Nindex = list(rows, cols),
                                             dim = dim(x))
            views <- Views(subject = x@rle, start = unlist(irl))
            val <- viewApply(X = views, FUN = prod, na.rm = na.rm)
            if (length(irl) == 0) {
              return(numeric(ncol(x)))
            }
            n <- length(irl[[1]])
            if (n == 1) {
              return(val)
            }
            IDX <- rep(seq_along(irl), each = n)
            # TODO: Obey top-level `useNames` argument.
            unlist(lapply(X = split(val, IDX), FUN = prod, na.rm = na.rm),
                   use.names = FALSE)
          }
)
