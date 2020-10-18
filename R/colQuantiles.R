### ============================================================================
### colQuantiles
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_colQuantiles <-
  function(x, rows = NULL, cols = NULL,
           probs = seq(from = 0, to = 1, by = 0.25), na.rm = FALSE, type = 7L,
           ..., drop = TRUE) {
    # Check input type
    stopifnot(is(x, "DelayedMatrix"))
    DelayedArray:::.get_ans_type(x, must.be.numeric = FALSE)

    # Subset
    x <- ..subset(x, rows, cols)

    # Compute result
    val <- colblock_APPLY(x = x,
                          FUN = colQuantiles,
                          probs = probs,
                          na.rm = na.rm,
                          type = type,
                          ...,
                          drop = drop)
    if (length(val) == 0L) {
      return(numeric(ncol(x)))
    }
    # NOTE: Return value of matrixStats::colQuantiles() is a vector if input is
    #       a column vector (matrix with 1 column);
    #       see https://github.com/HenrikBengtsson/matrixStats/issues/123
    if (ncol(x) == 1L) {
      return(unlist(val))
    }
    val <- do.call(rbind, val)
    rownames(val) <- colnames(x)
    val
  }

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit MatrixGenerics::colQuantiles
#' @importMethodsFrom DelayedArray seed
#' @rdname colQuantiles
#' @template common_params
#' @template lowercase_x
#' @export
#' @template example_dm_df
#' @author Peter Hickey
#' @examples
#'
#' # colnames, if present, are preserved as rownames on output
#' colQuantiles(dm_df)
setMethod("colQuantiles", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL,
                   probs = seq(from = 0, to = 1, by = 0.25), na.rm = FALSE,
                   type = 7L, force_block_processing = FALSE, ...,
                   drop = TRUE) {
            .smart_seed_dispatcher(x, generic = MatrixGenerics::colQuantiles, 
                                   blockfun = .DelayedMatrix_block_colQuantiles,
                                   force_block_processing = force_block_processing,
                                   rows = rows,
                                   cols = cols,
                                   probs = probs,
                                   na.rm = na.rm,
                                   # type = type, # TODO: wait for SMS to fix.
                                   ...,
                                   drop = drop)
          }
)
