### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

setMethod(
  "colsum",
  "ANY",
  function(x, group, reorder = TRUE, na.rm = FALSE...) {
    t(rowsum(t(x), group = group, reorder = reorder, na.rm = na.rm, ...))
  }
)

# ------------------------------------------------------------------------------
# Class-aware methods
#

# NOTE: Class-aware is a more restrictive definition than 'seed-aware'.

#' Give Column and Row Sums of an *HDF5Matrix* Based on a Grouping Variable
#'
#' @description Compute column and row sums across rows or columns of a numeric
#' [HDF5Array::HDF5Matrix] object for each level of a grouping variable.
#'
#' @details **NOTE**: Unlike [`base::rowsum()`], the result is a
#' [`base::double`] unless `type = "integer"` is specified. Notably, compared
#' to [`base::rowsum()`], this means that there are not the same issues with
#' over/underflow in forming the sum results for integer arguments.
#'
#' @param x An [HDF5Array::HDF5Matrix-class] object.
#' @inheritParams colsum
#' @param na.rm logical (`TRUE` or `FALSE`). Should `NA` (including `NaN`)
#' values be discarded?
#' @param filepath `NULL` or the path (as a single string) to the (new or
#' existing) HDF5 file where to write the dataset. If `NULL`, then the dataset
#' will be written to the current *HDF5 dump file* i.e. the path returned by
#' [`HDF5Array::getHDF5DumpFile()`] will be used.
#' @param name `NULL` or the name of the HDF5 dataset to write. If `NULL`, then
#' the name returned by `[HDF5Array::getHDF5DumpName()]` will be used.
#' @param chunkdim The dimensions of the chunks to use for writing the data to
#' disk. By default,
#' `HDF5Array::getHDF5DumpChunkDim(dim(ans))` will be
#' used, where `ans` is the returned object. See
#' `?`[`HDF5Array::getHDF5DumpChunkDim()`] for more information.
#' @param level The compression level to use for writing the data to disk. By
#' default, [`HDF5Array::getHDF5DumpCompressionLevel()`] will be used. See
#' `?`[`HDF5Array::getHDF5DumpCompressionLevel()`] for more information.
#' @param type The type of the data that will be written to the
#' [HDF5Array][HDF5RealizationSink-class] object to create the result. If the
#' result is known *a priori* to be `integer`, then it is recommended to set
#' `type = "integer"`.
#' @param BPPARAM An optional [BiocParallel][BiocParallelParam] instance
#' determining the parallel back-end to be used during evaluation, or a list of
#' [BiocParallel][BiocParallelParam] instances, to be applied in sequence for
#' nested calls to **BiocParallel** functions.
#'
#' @template example_dm_HDF5
#' @examples
#' group <- c(1, 1, 2)
#'
#' # Compute the sums and store them in an HDF5-backed DelayedMatrix.
#' xsum <- colsum(dm_HDF5, group)
#' class(seed(xsum))
#'
#' @importFrom BiocParallel bplapply bpparam ipcid ipclock ipcremove ipcunlock
#' @importFrom HDF5Array HDF5RealizationSink
#' @export
setMethod(
  "colsum",
  "HDF5Matrix",
  function(x, group, reorder = TRUE, na.rm = FALSE, filepath = NULL,
           name = NULL, chunkdim = NULL, level = NULL,
           type = c("double", "integer"), BPPARAM = bpparam()) {

    # Check arguments ----------------------------------------------------------

    type <- match.arg(type)
    if (any(!c(type(x), type) %in% c("integer", "double"))) {
      stop("'type(x)' must be 'integer' or 'double'.")
    }
    if (length(group) != NCOL(x)) {
      stop("incorrect length for 'group'")
    }
    if (anyNA(group)) {
      warning("missing values for 'group'")
    }
    ugroup <- unique(group)
    if (reorder) {
      ugroup <- sort(ugroup, na.last = TRUE, method = "quick")
    }
    # TODO: Default is type = "double" because rowSums2() returns numeric, but
    #       it can be useful to manually override this when you know the result
    #       is integer.

    # Construct RealizationSink --------------------------------------------

    # NOTE: This is ultimately coerced to the output DelayedMatrix
    #       object
    ans_nrow <- nrow(x)
    ans_ncol <- length(ugroup)
    ans_dim <- c(ans_nrow, ans_ncol)
    sink <- HDF5RealizationSink(
      dim = ans_dim,
      dimnames = list(rownames(x), as.character(ugroup)),
      type = type,
      filepath = filepath,
      name = name,
      chunkdim = chunkdim,
      level = level)
    sink_lock <- ipcid()
    on.exit(ipcremove(sink_lock), add = TRUE)

    # Construct ArrayGrid ------------------------------------------------------

    sink_grid <- RegularArrayGrid(refdim = dim(sink), spacings = c(nrow(x), 1L))
    list_of_cols <- split(seq_along(group), group)[ugroup]

    # Compute colsum() ---------------------------------------------------------

    bplapply(
      X = seq_along(sink_grid),
      FUN = function(b, x, sink, sink_lock, sink_grid, list_of_cols) {
        cols <- list_of_cols[[b]]
        if (length(cols) == 1L) {
          ans <- as.matrix(x[, cols, drop = FALSE])
          if (na.rm) {
            ans[is.na(ans)] <- 0L
          }
        } else {
          ans <- matrix(rowSums2(x, cols = cols, na.rm = na.rm), ncol = 1)
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
      list_of_cols = list_of_cols,
      BPPARAM = BPPARAM)
    as(sink, "DelayedArray")
  }
)
