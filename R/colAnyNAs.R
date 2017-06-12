### ============================================================================
### colAnyNAs
###

# ------------------------------------------------------------------------------
# Exported generics
#

#' @importFrom matrixStats colAnyNAs
#' @importFrom methods setMethod
#' @rdname colAnyNAs
#' @export
setMethod("colAnyNAs", "ANY",
          function(x, rows = NULL, cols = NULL, ...) {
            message2("ANY", get_verbose())
            colAnys(x, rows, cols, value = NA, ...)
          }
)
