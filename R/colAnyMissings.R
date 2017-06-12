### ============================================================================
### colAnyMissings
###

# ------------------------------------------------------------------------------
# Exported methods
#

# TODO: Should this be deprecated or defunct in initial release? Function is
#       softly deprecated in matrixStats but not formally.
#' @importFrom matrixStats colAnyMissings
#' @importFrom methods setMethod
#' @rdname colAnyMissings
#' @export
setMethod("colAnyMissings", "ANY",
          function(x, rows = NULL, cols = NULL, ...) {
            message2("ANY", get_verbose())
            .Deprecated("colAnyNAs")
            colAnyNAs(x, rows, cols, ...)
          }
)
