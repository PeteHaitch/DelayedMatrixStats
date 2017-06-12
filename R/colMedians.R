### ============================================================================
### colMedians
###

# ------------------------------------------------------------------------------
# Non-exported methods
#

# TODO: What to do with dim. arg?
#' Column medians of DelayedMatrix using block-processing method
#' @inherit matrixStats::colMedians
#' @importFrom matrixStats colMedians
#' @importFrom methods is
.DelayedMatrix_block_colMedians <- function(x, rows = NULL, cols = NULL,
                                            na.rm = FALSE, dim. = dim(x), ...) {
  stopifnot(is(x, "DelayedMatrix"))
  stopifnot(!x@is_transposed)

  # Check input type
  DelayedArray:::.get_ans_type(x)
  x <- ..subset(x, rows = rows, cols = cols)
  val <- DelayedArray:::colblock_APPLY(x,
                                       matrixStats::colMedians,
                                       na.rm = na.rm)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  # NOTE: Return value of matrixStats::colMedians() has no names
  unlist(val, recursive = FALSE, use.names = FALSE)
}

# ------------------------------------------------------------------------------
# Exported methods
#

#' @importFrom matrixStats colMedians
#' @importFrom methods setMethod
#' @rdname colMedians
#' @export
setMethod("colMedians", "matrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                   ...) {
            message2(class(x), get_verbose())
            matrixStats::colMedians(x, rows, cols, na.rm, dim., ...)
          }
)

#' @importFrom matrixStats colMedians
#' @importFrom methods setMethod
#' @rdname colMedians
#' @export
setMethod("colMedians", "Matrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                   ...) {
            message2(class(x), get_verbose())
            x <- ..subset(x, rows, cols)
            # NOTE: Return value of matrixStats::colMedians() has no names
            unname(apply(x, 2, median))
          }
)

#' @importFrom matrixStats colMedians
#' @importFrom methods setMethod
#' @rdname colMedians
#' @export
setMethod("colMedians", "data.frame",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                   ...) {
            message2(class(x), get_verbose())
            x <- ..subset(x, rows, cols)
            # NOTE: Return value of matrixStats::colMedians() has no names
            unname(apply(x, 2, median))
          }
)

#' @importFrom matrixStats colMedians
#' @importFrom methods setMethod
#' @rdname colMedians
#' @export
setMethod("colMedians", "DataFrame",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                   ...) {
            message2(class(x), get_verbose())
            x <- ..subset(x, rows, cols)
            # NOTE: Return value of matrixStats::colMedians() has no names
            unname(apply(x, 2, median))
          }
)

#' @importFrom DelayedArray seed
#' @rdname colMedians
#' @export
setMethod("colMedians", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                   ...) {
            if (x@is_transposed) {
              message2("Transposed", get_verbose())
              return(rowMedians(t(x), rows = cols, cols = rows, na.rm, dim.,
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
              return(colMedians(x, rows, cols, na.rm, dim., ...))
            } else {
              message2("Block processing", get_verbose())
              .DelayedMatrix_block_colMedians(x, rows, cols, na.rm, dim., ...)
            }
          }
)

# TODO: colMedians,HDF5Matrix-method
# TODO: colMedians,RleArraySeed-method and/or
#       colMedians,SolidRleArraySeed-method and/or
#       colMedians,ChunkedRleArraySeed-method

# TODO: ANY may be too general?
#' @importFrom DelayedArray DelayedArray
#' @importFrom methods setMethod
#' @rdname colMedians
#' @export
setMethod("colMedians", "ANY",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                   ...) {
            message2("ANY", get_verbose())
            x <- DelayedArray::DelayedArray(x)
            .DelayedMatrix_block_colMedians(x, rows, cols, na.rm, dim., ...)
          }
)