### =============================================================================
### rowsum
###

# TODO: Add `rows` and `cols` args?
.DelayedMatrix_block_rowsum <- function(x, group, reorder = TRUE,
                                        na.rm = FALSE, ...) {
  # Check input
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = TRUE)

  # Compute result
  val <- DelayedArray:::colblock_APPLY(x = x,
                                       APPLY = base::rowsum.default,
                                       group = group,
                                       reorder = reorder,
                                       na.rm = na.rm,
                                       ...)
  if (length(val) == 0L) {
    return(numeric(ncol(x)))
  }
  do.call("cbind", val)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' Give Column and Row Sums of an *DelayedMatrix* Based on a Grouping Variable
#'
#' @description Compute column and row sums across rows or columns of a numeric
#' [DelayedArray::DelayedMatrix] object for each level of a grouping variable
#' using block-processing.
#'
#' @param x An [DelayedArray::DelayedMatrix-class] object.
#' @inheritParams rowsum
#' @param na.rm logical (`TRUE` or `FALSE`). Should `NA` (including `NaN`)
#' values be discarded?
#' @template common_params
#' @importMethodsFrom DelayedArray seed
#' @export
#' @template example_dm_MatrixMatrix
#' @examples
#'
#' rowsum(dm_Matrix, group = c(1, 1, 1, 2, 2))
setMethod("rowsum", "DelayedMatrix",
          function(x, group, reorder = TRUE, na.rm = FALSE,
                   force_block_processing = FALSE, ...) {
            if (!existsMethod("rowsum", seedClass(x)) ||
                force_block_processing) {
              message2("Block processing", get_verbose())
              return(.DelayedMatrix_block_rowsum(x = x,
                                                 group = group,
                                                 reorder = reorder,
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
                return(rowsum(x = x,
                              group = group,
                              reorder = reorder,
                              na.rm = na.rm,
                              force_block_processing = TRUE,
                              ...))
              }
            }

            rowsum(x = simple_seed_x,
                   group = group,
                   reorder = reorder,
                   na.rm = na.rm,
                   ...)
          }
)

# ------------------------------------------------------------------------------
# Seed-aware methods
#

#' @export
setMethod("rowsum", "matrix", base::rowsum)

# ------------------------------------------------------------------------------
# Class-aware methods
#

# NOTE: Class-aware is a more restrictive definition than 'seed-aware'.

#' @importFrom BiocParallel bplapply bpparam ipcid ipclock ipcremove ipcunlock
#' @importFrom DelayedArray rowGrid
#' @importFrom HDF5Array HDF5RealizationSink
#' @rdname colsum-HDF5Matrix-method
#' @export
setMethod(
  "rowsum",
  "HDF5Matrix",
  function(x, group, reorder = TRUE, na.rm = FALSE, filepath = NULL,
           name = NULL, chunkdim = NULL, level = NULL,
           type = c("double", "integer"), BPPARAM = bpparam()) {

    # Check arguments ----------------------------------------------------------

    if (any(!c(type(x), type) %in% c("integer", "double"))) {
      stop("'type(x)' must be 'integer' or 'double'.")
    }
    if (length(group) != NROW(x)) {
      stop("incorrect length for 'group'")
    }
    if (anyNA(group)) {
      warning("missing values for 'group'")
    }
    ugroup <- unique(group)
    if (reorder) {
      ugroup <- sort(ugroup, na.last = TRUE, method = "quick")
    }
    # NOTE: Default is type = "double" because colSums2() returns numeric, but
    #       it can be useful to manually override this when you know the result
    #       is integer.
    type <- match.arg(type)

    # Construct RealizationSink --------------------------------------------

    # NOTE: This is ultimately coerced to the output DelayedMatrix
    #       object
    ans_nrow <- length(ugroup)
    ans_ncol <- ncol(x)
    ans_dim <- c(ans_nrow, ans_ncol)
    sink <- HDF5RealizationSink(
      dim = ans_dim,
      dimnames = list(as.character(ugroup), colnames(x)),
      type = type,
      filepath = filepath,
      name = name,
      chunkdim = chunkdim,
      level = level)
    sink_lock <- ipcid()
    on.exit(ipcremove(sink_lock), add = TRUE)

    # Construct ArrayGrid ------------------------------------------------------

    sink_grid <- rowGrid(x = sink, nrow = 1L)
    list_of_rows <- split(seq_along(group), group)[as.character(ugroup)]

    # Compute colsum() ---------------------------------------------------------

    bplapply(
      X = seq_along(sink_grid),
      FUN = function(b, x, sink, sink_lock, sink_grid, list_of_rows) {
        rows <- list_of_rows[[b]]
        if (length(rows) == 1L) {
          ans <- as.matrix(x[rows, , drop = FALSE])
          if (na.rm) {
            ans[is.na(ans)] <- 0L
          }
        } else {
          ans <- matrix(colSums2(x, rows = rows, na.rm = na.rm), nrow = 1)
        }
        ipclock(sink_lock)
        write_block(x = sink, viewport = sink_grid[[b]], block = ans)
        ipcunlock(sink_lock)
        NULL
      },
      x = x,
      sink = sink,
      sink_lock = sink_lock,
      sink_grid = sink_grid,
      list_of_rows = list_of_rows,
      BPPARAM = BPPARAM)
    as(sink, "DelayedArray")
  }
)
