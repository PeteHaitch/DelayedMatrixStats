### ============================================================================
### rowCollapse
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

#' `rowCollapse()` block-processing internal helper
#' @inherit matrixStats::rowCollapse
#' @importMethodsFrom DelayedArray t
.DelayedMatrix_block_rowCollapse <- function(x, idxs, rows = NULL,
                                             dim. = dim(x), ...) {
  # Check input
  stopifnot(is(x, "DelayedMatrix"))
  stopifnot(!x@is_transposed)
  DelayedArray:::.get_ans_type(x, must.be.numeric = FALSE)

  # Subset
  x <- ..subset(x, rows = rows)
  if (!is.null(rows)) {
    idxs <- idxs[rows]
  }

  # Compute result
  # NOTE: Use colCollapse() on transposed input
  val <- DelayedArray:::colblock_APPLY(x = t(x),
                                       APPLY = matrixStats::colCollapse,
                                       idxs = idxs,
                                       ...)

  if (length(val) == 0L) {
    return(numeric(nrow(x)))
  }
  # NOTE: Return value of matrixStats::rowCollapse() has no names
  unlist(val, recursive = TRUE, use.names = FALSE)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @importMethodsFrom DelayedArray seed
#' @rdname colCollapse
#' @export
#' @examples
#'
#' # Extract the 2nd column as a vector
#' # NOTE: An ordinary vector is returned regardless of the backend of
#' #       the DelayedMatrix object
#' rowCollapse(dm_matrix, 2)
#' rowCollapse(dm_HDF5, 2)
setMethod("rowCollapse", "DelayedMatrix",
          function(x, idxs, rows = NULL, dim. = dim(x),
                   force_block_processing = FALSE, ...) {
            if (!hasMethod("rowCollapse", class(seed(x))) ||
                force_block_processing) {
              message2("Block processing", get_verbose())
              return(.DelayedMatrix_block_rowCollapse(x = x,
                                                      idxs = idxs,
                                                      rows = rows,
                                                      dim. = dim.,
                                                      ...))
            }

            message2("Has seed-aware method", get_verbose())
            if (DelayedArray:::is_pristine(x)) {
              message2("Pristine", get_verbose())
              simple_seed_x <- seed(x)
            } else {
              message2("Coercing to seed class", get_verbose())
              # TODO: do_transpose trick
              simple_seed_x <- try(from_DelayedArray_to_simple_seed_class(x),
                                   silent = TRUE)
              if (is(simple_seed_x, "try-error")) {
                message2("Unable to coerce to seed class", get_verbose())
                return(rowCollapse(x = x,
                                   idxs = idxs,
                                   rows = rows,
                                   dim. = dim.,
                                   force_block_processing = TRUE,
                                   ...))
              }
            }

            rowCollapse(x = simple_seed_x,
                        idxs = idxs,
                        rows = rows,
                        dim. = dim.,
                        ...)
          }
)

# ------------------------------------------------------------------------------
# Seed-aware methods
#

#' @export
setMethod("rowCollapse", "matrix", matrixStats::rowCollapse)
