### =============================================================================
### rowAnyMissings
###

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @rdname DelayedMatrixStats-deprecated
#' @export
setMethod("rowAnyMissings", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, force_block_processing = FALSE,
                   ..., useNames = TRUE) {
            rowAnyNAs(x = x,
                    rows = rows,
                    cols = cols,
                    force_block_processing = force_block_processing,
                    ...,
                    useNames = useNames)
          }
)

# ------------------------------------------------------------------------------
# Seed-aware methods
#

#' @rdname DelayedMatrixStats-deprecated
#' @export
setMethod("rowAnyMissings", "matrix",
          function(x, rows = NULL, cols = NULL, ..., useNames = TRUE) {
            rowAnyNAs(x = x,
                      rows = rows,
                      cols = cols,
                      ...,
                      useNames = useNames)
          }
)
