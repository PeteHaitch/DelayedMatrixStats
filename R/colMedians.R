### ============================================================================
### colMedians
###

# ------------------------------------------------------------------------------
# Non-exported methods
#

#' Column medians of DelayedMatrix using block-processing method
#' @inherit matrixStats::colMedians
#' @importFrom matrixStats colMedians
.DelayedMatrix_block_colMedians <- function(x, rows = NULL, cols = NULL,
                                            na.rm = FALSE, dim. = dim(x), ...) {
  stopifnot(is(x, "DelayedMatrix"))
  if (x@is_transposed) {
    return(.DelayedMatrix_block_rowMedians(t(x), rows = cols, cols = rows,
                                           na.rm, dim., ...))
  }

  # Check input type
  DelayedArray:::.get_ans_type(x)
  x <- ..subset(x, rows = rows, cols = cols)
  colmedians_list <- DelayedArray:::colblock_APPLY(x,
                                                   matrixStats::colMedians,
                                                   na.rm = na.rm)
  if (length(colmedians_list) == 0L)
    return(numeric(ncol(x)))
  # NOTE: Return value of matrixStats::colMedians() has no names
  unlist(colmedians_list, recursive = FALSE, use.names = FALSE)
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
            # unlist(lapply(seq_len(ncol(x)), function(j) {
            #   median(x[, j])
            # }), use.names = FALSE)
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
            # unlist(lapply(seq_len(ncol(x)), function(j) {
            #   median(x[, j])
            # }), use.names = FALSE)
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
            # unlist(lapply(seq_len(ncol(x)), function(j) {
            #   median(x[, j])
            # }), use.names = FALSE)
            unname(apply(x, 2, median))
          }
)

#' @importFrom DelayedArray seed
#' @importFrom methods is
#' @importFrom stats median
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
#' @importFrom matrixStats colMedians
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