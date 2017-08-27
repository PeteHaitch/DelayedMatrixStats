#' @importFrom methods setGeneric
#' @inherit matrixStats::colWeightedSds
#' @export
setGeneric("colWeightedSds", signature = "x",
           function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE,
                    ...) standardGeneric("colWeightedSds")
)

#' @importFrom methods setGeneric
#' @inherit matrixStats::colWeightedVars
#' @export
setGeneric("colWeightedVars", signature = "x",
           function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE,
                    ...) standardGeneric("colWeightedVars")
)


#' @importFrom methods setGeneric
#' @rdname colWeightedSds
#' @export
setGeneric("rowWeightedSds", signature = "x",
           function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE,
                    ...) standardGeneric("rowWeightedSds")
)

#' @importFrom methods setGeneric
#' @rdname colWeightedVars
#' @export
setGeneric("rowWeightedVars", signature = "x",
           function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE,
                    ...) standardGeneric("rowWeightedVars")
)
