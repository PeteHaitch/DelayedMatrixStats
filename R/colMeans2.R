### ============================================================================
### colMeans2
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_colMeans2 <- function(x, rows = NULL, cols = NULL,
                                           na.rm = FALSE, ...) {
  # Check input
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = TRUE)

  # Subset
  x <- ..subset(x, rows, cols)

  # Compute result
  val <- colblock_APPLY(x = x,
                        FUN = matrixStats::colMeans2,
                        na.rm = na.rm,
                        ...)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  # NOTE: Return value of matrixStats::colMeans2() has no names
  unlist(val, recursive = FALSE, use.names = FALSE)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit MatrixGenerics::colMeans2
#' @importMethodsFrom DelayedArray seed
#' @rdname colMeans2
#' @template common_params
#' @template lowercase_x
#' @export
#' @template example_dm_matrix
#' @template example_dm_Rle
#' @author Peter Hickey
#' @examples
#'
#' colMeans2(dm_matrix)
setMethod("colMeans2", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, 
                   force_block_processing = FALSE, ...) {
            .smart_seed_dispatcher(x, generic = "colMeans2", 
                                   blockfun = .DelayedMatrix_block_colMeans2,
                                   force_block_processing = force_block_processing,
                                   rows = rows,
                                   cols = cols,
                                   na.rm = na.rm,
                                   ...)
          }
)

# ------------------------------------------------------------------------------
# Seed-aware methods
#

#' @importMethodsFrom Matrix colMeans
#' @rdname colMeans2
#' @export
setMethod("colMeans2", "Matrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, 
                   ...) {
            message2(class(x), get_verbose())
            x <- ..subset(x, rows, cols)
            # NOTE: Return value of matrixStats::colMeans2() has no names
            unname(colMeans(x = x, na.rm = na.rm))
          }
)

#' @importMethodsFrom IRanges Views viewMeans
#' @rdname colMeans2
#' @export
setMethod("colMeans2", "SolidRleArraySeed",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, 
                   ...) {
            message2(class(x), get_verbose())
            irl <- get_Nindex_as_IRangesList(Nindex = list(rows, cols),
                                             dim = dim(x))
            views <- Views(subject = x@rle, start = unlist(irl))
            val <- viewMeans(x = views, na.rm = na.rm)
            if (length(irl) == 0) {
              return(val)
            }
            n <- length(irl[[1]])
            if (n == 1) {
              return(val)
            }
            IDX <- rep(seq_along(irl), each = n)
            unlist(lapply(X = split(val, IDX), FUN = mean, na.rm = na.rm),
                   use.names = FALSE)
          }
)
