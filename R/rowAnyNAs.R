### =============================================================================
### rowAnyNAs
###

### ----------------------------------------------------------------------------
### Exported methods
###

.DelayedMatrix_block_rowAnyNAs <- function(x, rows, cols, ..., useNames = NA) {
    rowAnys(x = x, rows = rows, cols = cols, value = NA, ..., useNames = useNames)
}

# ------------------------------------------------------------------------------
# General method
#

#' @inherit MatrixGenerics::rowAnyNAs
#' @importMethodsFrom DelayedArray seed
#' @rdname colAnyNAs
#' @export
setMethod("rowAnyNAs", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, force_block_processing = FALSE,
                   ..., useNames = NA) {
            .smart_seed_dispatcher(x, generic = MatrixGenerics::rowAnyNAs,
                                   blockfun = .DelayedMatrix_block_rowAnyNAs,
                                   force_block_processing = force_block_processing,
                                   rows = rows,
                                   cols = cols,
                                   ...,
                                   useNames = useNames)
          }
)
