### ============================================================================
### rowQuantiles
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_rowQuantiles <-
  function(x, rows = NULL, cols = NULL,
           probs = seq(from = 0, to = 1, by = 0.25), na.rm = FALSE, type = 7L,
           ..., useNames = NA, drop = TRUE) {
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
                          useNames = useNames,
                          drop = FALSE)

    if (length(val) == 0L) {
      return(numeric(ncol(x)))
    }

    # TODO: Obey top-level `useNames` argument.
    val <- do.call(rbind, val)
    rownames(val) <- rownames(x)

    if (drop && any(dim(val)==1L)) {
      return(drop(val))
    }
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
                   type = 7L, force_block_processing = FALSE, ..., useNames = NA,
                   drop = TRUE) {
            .smart_seed_dispatcher(x, generic = MatrixGenerics::rowQuantiles,
                                   blockfun = .DelayedMatrix_block_rowQuantiles,
                                   force_block_processing = force_block_processing,
                                   rows = rows,
                                   cols = cols,
                                   probs = probs,
                                   na.rm = na.rm,
                                   # type = type, TODO: wait for SMS to fix.
                                   ...,
                                   useNames = useNames,
                                   drop = drop)
          }
)
