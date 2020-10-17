### ============================================================================
### rowQuantiles
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_rowQuantiles <-
  function(x, rows = NULL, cols = NULL,
           probs = seq(from = 0, to = 1, by = 0.25), na.rm = FALSE, type = 7L,
           ..., drop = TRUE) {
    # Check input type
    stopifnot(is(x, "DelayedMatrix"))
    DelayedArray:::.get_ans_type(x, must.be.numeric = FALSE)

    # Subset
    x <- ..subset(x, rows, cols)

    # Compute result
    val <- rowblock_APPLY(x = x,
                          FUN = rowQuantiles,
                          probs = probs,
                          na.rm = na.rm,
                          type = type,
                          ...,
                          drop = drop)
    if (length(val) == 0L) {
      return(numeric(ncol(x)))
    }
    # NOTE: Return value of matrixStats::rowQuantiles() is a vector if input is
    #       a row vector (matrix with 1 row);
    #       see https://github.com/HenrikBengtsson/matrixStats/issues/123
    if (nrow(x) == 1L) {
      return(unlist(val))
    }
    val <- do.call(rbind, val)
    rownames(val) <- rownames(x)
    val
  }

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit MatrixGenerics::rowQuantiles
#' @importMethodsFrom DelayedArray seed
#' @rdname colQuantiles
#' @export
#' @examples
#'
#' # Input has no rownames so output has no rownames
#' rowQuantiles(dm_df)
setMethod("rowQuantiles", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL,
                   probs = seq(from = 0, to = 1, by = 0.25), na.rm = FALSE,
                   type = 7L, force_block_processing = FALSE, ...,
                   drop = TRUE) {
            .smart_seed_dispatcher(x, generic = "rowQuantiles", 
                                   blockfun = .DelayedMatrix_block_rowQuantiles,
                                   force_block_processing = force_block_processing,
                                   rows = rows,
                                   cols = cols,
                                   probs = probs,
                                   na.rm = na.rm,
                                   # type = type, TODO: wait for SMS to fix.
                                   ...,
                                   drop = drop)
          }
)
