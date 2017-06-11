### =============================================================================
### colSums2
###

# ------------------------------------------------------------------------------
# Exported methods
#

#' @importFrom matrixStats colSums2
#' @importFrom methods setMethod
#' @rdname colSums2
#' @export
setMethod("colSums2", "matrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                   ...) {
            message2(class(x), get_verbose())
            matrixStats::colSums2(x, rows, cols, na.rm, dim., ...)
          }
)

# NOTE: No different from colSums2,ANY-method except that it explicitly calls
#       Matrix::colSums() to resolve what I think is a namespace collision with
#       BiocGenerics::colSums()
#' @importFrom methods setMethod
#' @rdname colSums2
#' @export
setMethod("colSums2", "Matrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                   ...) {
            message2(class(x), get_verbose())
            x <- ..subset(x, rows, cols)
            # NOTE: Return value of matrixStats::colSums2() has no names
            unname(Matrix::colSums(x, na.rm))
          }
)

#' @importFrom IRanges Views viewSums
#' @rdname colSums2
#' @export
setMethod("colSums2", "RleArraySeed",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                   ...) {
            message2(class(x), get_verbose())
            irl <- get_Nindex_as_IRangesList(list(rows, cols), dim(x))
            views <- IRanges::Views(x@rle, unlist(irl))
            val <- IRanges::viewSums(views, na.rm)
            if (length(irl) == 0) {
              return(val)
            }
            n <- length(irl[[1]])
            if (n == 1) {
              return(val)
            }
            IDX <- rep(seq_along(irl), each = n)
            unlist(lapply(split(val, IDX), sum, na.rm), use.names = FALSE)
          }
)

#' @importFrom BiocGenerics colSums
#' @importFrom DelayedArray seed
#' @importFrom methods is
#' @rdname colSums2
#' @export
setMethod("colSums2", "DelayedMatrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                   ...) {
            if (.has_simple_seed(x)) {
              message2("Simple seed", get_verbose())
              if (DelayedArray:::is_pristine(x)) {
                message2("Pristine", get_verbose())
                x <- seed(x)
              } else {
                message2("Coercing to seed class", get_verbose())
                # TODO: do_transpose trick
                x <- from_DelayedArray_to_simple_seed_class(x)
              }
              return(colSums2(x, rows, cols, na.rm, dim., ...))
            } else {
              message2("Block processing", get_verbose())
              x <- ..subset(x, rows, cols)
              # TODO: Check dims.?
              # NOTE: Return value of matrixStats::colSums2() has no names
              unname(DelayedArray::colSums(x, na.rm))
            }
          }
)

# TODO: colSums2,DataFrame-method?
# TODO: colSums2,data.frame-method?
# TODO: colSums2,HDF5Matrix-method
# TODO: colSums2,SolidRleArraySeed-method and/or
#       colSums2,ChunkedRleArraySeed-method

# TODO: ANY may be too general?
# NOTE: Used by data.frame, DataFrame, HDF5ArraySeed, and SeedBinder seed. This
#       feels a little circular since it takes an ANY (those for which we
#       don't have an explicit colSums2() method) as a seed, contructs an
#       DelayedMatrix from that seed, subsets the DelayedMatrix, and then calls
#       colSums,DelayedMatrix-method. All of this is to avoid writing an
#       explicit colSums2() method for these classes of seeds, for which it is
#       likely to be difficult to write an implementation that is more
#       efficient or simpler than the "block processing" strategy used by
#       colSums,DelayedMatrix-method
#' @importFrom DelayedArray DelayedArray
#' @importFrom methods setMethod
#' @rdname colSums2
#' @export
setMethod("colSums2", "ANY",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                   ...) {
            message2("ANY", get_verbose())
            x <- DelayedArray::DelayedArray(x)
            x <- ..subset(x, rows, cols)
            # NOTE: Return value of matrixStats::colSums2() has no names
            unname(colSums(x, na.rm))
          }
)
