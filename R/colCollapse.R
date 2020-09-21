### ============================================================================
### colCollapse
###

### ----------------------------------------------------------------------------
### Non-exported methods
###

.DelayedMatrix_block_colCollapse <- function(x, idxs, cols = NULL,
                                             dim. = dim(x), ...) {
  # Check input
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = FALSE)

  # Subset
  x <- ..subset(x, cols = cols)
  if (!is.null(cols)) {
    idxs <- idxs[cols]
  }

  # Compute result
  # NOTE: This uses a hacky implementation of DelayedArray:::colblock_MAPPLY()
  IDXS <- rep_len(idxs, ncol(x))
  val <- lapply(seq_along(IDXS), function(i) {
    DelayedArray:::colblock_APPLY(x = x[, i, drop = FALSE],
                                  APPLY = matrixStats::colCollapse,
                                  idxs = IDXS[i],
                                  ...)
  })
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  # NOTE: Return value of matrixStats::colCollapse() has no names
  unlist(val, recursive = TRUE, use.names = FALSE)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @inherit MatrixGenerics::colCollapse
#' @importMethodsFrom DelayedArray seed
#' @rdname colCollapse
#' @template common_params
#' @template lowercase_x
#' @export
#' @template example_dm_matrix
#' @template example_dm_HDF5
#' @author Peter Hickey
#' @examples
#'
#' # Extract the 4th row as a vector
#' # NOTE: An ordinary vector is returned regardless of the backend of
#' #       the DelayedMatrix object
#' colCollapse(dm_matrix, 4)
#' colCollapse(dm_HDF5, 4)
setMethod("colCollapse", "DelayedMatrix",
          function(x, idxs, cols = NULL, dim. = dim(x),
                   force_block_processing = FALSE, ...) {
            if (!hasMethod("colCollapse", seedClass(x)) ||
                force_block_processing) {
              message2("Block processing", get_verbose())
              return(.DelayedMatrix_block_colCollapse(x = x,
                                                      idxs = idxs,
                                                      cols = cols,
                                                      dim. = dim.,
                                                      ...))
            }

            message2("Has seed-aware method", get_verbose())
            if (isPristine(x)) {
              message2("Pristine", get_verbose())
              simple_seed_x <- seed(x)
            } else {
              message2("Coercing to seed class", get_verbose())
              # TODO: do_transpose trick
              simple_seed_x <- try(from_DelayedArray_to_simple_seed_class(x),
                                   silent = TRUE)
              if (is(simple_seed_x, "try-error")) {
                message2("Unable to coerce to seed class", get_verbose())
                return(colCollapse(x = x,
                                   idxs = idxs,
                                   cols = cols,
                                   dim. = dim.,
                                   force_block_processing = TRUE,
                                   ...))
              }
            }

            colCollapse(x = simple_seed_x,
                        idxs = idxs,
                        cols = cols,
                        dim. = dim.,
                        ...)
          }
)
