### ============================================================================
### colCollapse
###

# ------------------------------------------------------------------------------
# Non-exported methods
#

# TODO: What to do with dim. arg?
#' Column 'alls' of DelayedMatrix using block-processing method
#' @inherit matrixStats::colCollapse
#' @importFrom matrixStats colCollapse
#' @importFrom methods is
.DelayedMatrix_block_colCollapse <- function(x, idxs, cols = NULL,
                                             dim. = dim(x), ...) {
  stopifnot(is(x, "DelayedMatrix"))
  stopifnot(!x@is_transposed)

  # Check input type
  DelayedArray:::.get_ans_type(x)
  x <- ..subset(x, cols = cols)
  val <- DelayedArray:::colblock_APPLY(x,
                                       matrixStats::colCollapse,
                                       idxs = idxs)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  # NOTE: Return value of matrixStats::colCollapse() has no names
  unlist(val, recursive = FALSE, use.names = FALSE)
}

# ------------------------------------------------------------------------------
# Exported methods
#

#' @importFrom matrixStats colCollapse
#' @importFrom methods setMethod
#' @rdname colCollapse
#' @export
setMethod("colCollapse", "matrix",
          function(x, idxs, cols = NULL, dim. = dim(x), ...) {
            message2(class(x), get_verbose())
            matrixStats::colCollapse(x, idxs, cols, dim., ...)
          }
)

#' @importFrom DelayedArray seed
#' @rdname colCollapse
#' @export
setMethod("colCollapse", "DelayedMatrix",
          function(x, idxs, cols = NULL, dim. = dim(x), ...) {
            if (x@is_transposed) {
              message2("Transposed", get_verbose())
              stop("Not yet implemented")
              # TODO: How to pass to rowCollapse()?
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
              return(colCollapse(x, idxs, cols, dim., ...))
            } else {
              message2("Block processing", get_verbose())
              .DelayedMatrix_block_colCollapse(x, idxs, cols, dim., ...)
            }
          }
)

# TODO: Additional colCollapse() methods

# TODO: ANY may be too general?
#' @importFrom DelayedArray DelayedArray
#' @importFrom methods setMethod
#' @rdname colCollapse
#' @export
setMethod("colCollapse", "ANY",
          function(x, idxs, cols = NULL, dim. = dim(x), ...) {
            message2("ANY", get_verbose())
            x <- DelayedArray::DelayedArray(x)
            .DelayedMatrix_block_colCollapse(x, idxs, cols, dim., ...)
          }
)
