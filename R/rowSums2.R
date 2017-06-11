### =============================================================================
### rowSums2
###

# ------------------------------------------------------------------------------
# Exported methods
#

#' @importFrom matrixStats rowSums2
#' @importFrom methods setMethod
#' @rdname rowSums2
#' @export
setMethod("rowSums2", "matrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                   ...) {
            message2(class(x), get_verbose())
            matrixStats::rowSums2(x, rows, cols, na.rm, dim., ...)
          }
)

# NOTE: No different from rowSums2,ANY-method except that it explicitly calls
#       Matrix::rowSums() to resolve what I think is a namespace collision with
#       BiocGenerics::rowSums()
#' @importFrom methods setMethod
#' @rdname rowSums2
#' @export
setMethod("rowSums2", "Matrix",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                   ...) {
            message2(class(x), get_verbose())
            x <- ..subset(x, rows, cols)
            # NOTE: Return value of matrixStats::rowSums2() has no names
            unname(Matrix::rowSums(x, na.rm))
          }
)

#' @importFrom BiocGenerics rowSums
#' @importFrom DelayedArray seed
#' @importFrom methods is
#' @rdname rowSums2
#' @export
setMethod("rowSums2", "DelayedMatrix",
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
              return(rowSums2(x, rows, cols, na.rm, dim., ...))
            } else {
              message2("Block processing", get_verbose())
              x <- ..subset(x, rows, cols)
              # TODO: Check dims.?
              # NOTE: Return value of matrixStats::colSums2() has no names
              unname(DelayedArray::rowSums(x, na.rm))
            }
          }
)

# TODO: rowSums2,HDF5Matrix-method
# TODO: rowSums2,RleArraySeed-method and/or
#       rowSums2,SolidRleArraySeed-method and/or
#       rowSums2,ChunkedRleArraySeed-method ?

# NOTE: This feels a little circular since it takes an ANY (those for which we
#       don't have an explicit rowSums2() method) as a seed, contructs an
#       DelayedMatrix from that seed, subsets the DelayedMatrix, and then calls
#       rowSums,DelayedMatrix-method. All of this is to avoid writing an
#       explicit rowSums2() method for these classes of seeds, for which it is
#       likely to be difficult to write an implementation that is more
#       efficient or simpler than the "block processing" strategy used by
#       rowSums,DelayedMatrix-method
#' @importFrom DelayedArray DelayedArray
#' @importFrom methods setMethod
#' @rdname rowSums2
#' @export
setMethod("rowSums2", "ANY",
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
                   ...) {
            message2("ANY", get_verbose())
            if (length(x) == 0L) {
              return(numeric(0L))
            }
            # TODO: Can the subset be moved before the DelayedArray()? Probably
            #       not (e.g. if x is a HDF5ArraySeed then cannot ..subset(x))
            x <- DelayedArray::DelayedArray(x)
            x <- ..subset(x, rows, cols)
            # NOTE: Return value of matrixStats::rowSums2() has no names
            unname(rowSums(x, na.rm))
          }
)
