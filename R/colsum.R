### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' Give Column/Row Sums of a Matrix or Data Frame Based on a Grouping Variable
#'
#' @description Compute column (row) sums across rows (columns) of a numeric
#' matrix-like object for each level of a grouping variable. `colsum()` and
#' `rowsum()` are generic, with a method for \linkS4class{DelayedMatrix},
#' data frames, and a default method for vectors and matrices.
#' @rdname colsum
#' @template common_params
#' @template lowercase_x
#' @template example_dm_HDF5
#' @export
#' @examples
#'
#' colsum(dm_HDF5, group = c(1, 1, 2))
setMethod("colsum", "ANY",
          function(x, group, reorder = TRUE, ...) {
            t(rowsum(t(x), group = group, reorder = reorder, ...))
          }
)
