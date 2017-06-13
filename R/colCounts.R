### ============================================================================
### colCounts
###

# ------------------------------------------------------------------------------
# Non-exported methods
#

# TODO: What to do with dim. arg?
#' Column 'anys' of DelayedMatrix using block-processing method
#' @inherit matrixStats::colCounts
#' @importFrom matrixStats colCounts
#' @importFrom methods is
.DelayedMatrix_block_colCounts <- function(x, rows = NULL, cols = NULL,
                                         value = TRUE, na.rm = FALSE,
                                         dim. = dim(x), ...) {
  stopifnot(is(x, "DelayedMatrix"))
  stopifnot(!x@is_transposed)

  # Check input type
  DelayedArray:::.get_ans_type(x)
  x <- ..subset(x, rows = rows, cols = cols)
  val <- DelayedArray:::colblock_APPLY(x,
                                       matrixStats::colCounts,
                                       value = value,
                                       na.rm = na.rm)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  # NOTE: Return value of matrixStats::colCounts() has no names
  unlist(val, recursive = FALSE, use.names = FALSE)
}

# ------------------------------------------------------------------------------
# Exported methods
#

#' @importFrom matrixStats colCounts
#' @importFrom methods setMethod
#' @rdname colCounts
#' @export
setMethod("colCounts", "matrix",
          function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE,
                   dim. = dim(x), ...) {
            message2(class(x), get_verbose())
            matrixStats::colCounts(x, rows, cols, value, na.rm, dim., ...)
          }
)

#' @importFrom DelayedArray seed
#' @rdname colCounts
#' @export
setMethod("colCounts", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE,
                   dim. = dim(x), ...) {
            if (x@is_transposed) {
              message2("Transposed", get_verbose())
              return(rowCounts(t(x), rows = cols, cols = rows, value, na.rm,
                             dim., ...))
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
              return(colCounts(x, rows, cols, value, na.rm, dim., ...))
            } else {
              message2("Block processing", get_verbose())
              .DelayedMatrix_block_colCounts(x, rows, cols, value, na.rm, dim.,
                                           ...)
            }
          }
)

# TODO: Additional colCounts() methods

# TODO: ANY may be too general?
#' @importFrom DelayedArray DelayedArray
#' @importFrom methods setMethod
#' @rdname colCounts
#' @export
setMethod("colCounts", "ANY",
          function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE,
                   dim. = dim(x), ...) {
            message2("ANY", get_verbose())
            x <- DelayedArray::DelayedArray(x)
            .DelayedMatrix_block_colCounts(x, rows, cols, value, na.rm, dim.,
                                           ...)
          }
)
