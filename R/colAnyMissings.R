### ============================================================================
### colAnyMissings
###

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#


#' @rdname DelayedMatrixStats-deprecated
#' @export
setMethod("colAnyMissings", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, force_block_processing = FALSE,
                   ..., useNames = TRUE) {
            colAnyNAs(x = x,
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
setMethod("colAnyMissings", "matrix",
          function(x, rows = NULL, cols = NULL, ..., useNames = TRUE) {
            colAnyNAs(x = x,
                      rows = rows,
                      cols = cols,
                      ...,
                      useNames = useNames)
          }
)
