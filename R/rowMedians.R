### ============================================================================
### rowMedians
###

# ------------------------------------------------------------------------------
# Non-exported methods
#

# NOTE: Hard to compute row medians using column blocks unless all rows fit
#       within a single column block. This means no (column) block-processing
#       strategy for row medians. Instead, iterate over rows and compute medians

# TODO: What to do with dim. arg?
#' Row medians of DelayedMatrix using block-processing method
#' @inherit matrixStats::rowMedians
#' @importFrom DelayedArray apply t
#' @importFrom matrixStats rowMedians
.DelayedMatrix_apply_rowMedians <- function(x, rows = NULL, cols = NULL,
                                            na.rm = FALSE, dim. = dim(x), ...) {
  stopifnot(is(x, "DelayedMatrix"))
  stopifnot(!x@is_transposed)

  # Check input type
  DelayedArray:::.get_ans_type(x)
  x <- ..subset(x, rows = rows, cols = cols)
  # NOTE: Return value of matrixStats::rowMedians() has no names
  # TODO: Performance of this is going to be **awful**; need to iterate rows in
  #       chunks and probably do so at C-level
  unname(apply(x, 1, function(xx) {
    matrixStats::rowMedians(matrix(xx, nrow = 1))
    }))
}

# ------------------------------------------------------------------------------
# Exported methods
#

#' @importFrom matrixStats colMedians
#' @importFrom methods setMethod
#' @rdname rowMedians
#' @export
setMethod("rowMedians", "matrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                   ...) {
            message2(class(x), get_verbose())
            matrixStats::rowMedians(x, rows, cols, na.rm, dim., ...)
          }
)

#' @importFrom matrixStats colMedians
#' @importFrom methods setMethod
#' @rdname rowMedians
#' @export
setMethod("rowMedians", "Matrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                   ...) {
            message2(class(x), get_verbose())
            x <- ..subset(x, rows, cols)
            # NOTE: Return value of matrixStats::colMedians() has no names
            # TODO: May be faster to do unname(apply(t(x), 2, median))
            unname(apply(x, 1, median))
          }
)

#' @importFrom DelayedArray seed
#' @importFrom methods is
#' @importFrom stats median
#' @rdname rowMedians
#' @export
setMethod("rowMedians", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                   ...) {
            if (x@is_transposed) {
              message2("Transposed", get_verbose())
              return(colMedians(t(x), rows = cols, cols = rows, na.rm, dim.,
                                ...))
            }
            if (.has_simple_seed(x)) {
              message2("Simple seed", get_verbose())
              if (DelayedArray:::is_pristine(x)) {
                message2("Pristine", get_verbose())
                x <- seed(x)
              } else {
                message2("Coercing to seed class", get_verbose())
                x <- from_DelayedArray_to_simple_seed_class(x)
              }
              return(rowMedians(x, rows, cols, na.rm, dim., ...))
            } else {
              message2("Block processing", get_verbose())
              .DelayedMatrix_apply_rowMedians(x, rows, cols, na.rm, dim., ...)
            }
          }
)