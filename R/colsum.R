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

# ------------------------------------------------------------------------------
# Seed-aware methods
#

#' @rdname colsum
#' @export
setMethod(
  "colsum",
  "HDF5Matrix",
  function(x, group, reorder = TRUE, filepath = NULL, name = NULL,
           chunkdim = NULL, level = NULL, type = c("double", "integer"),
           BPPARAM = bpparam()) {

    # Check arguments ----------------------------------------------------------

    if (!type(x) %in% c("integer", "double")) {
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
    type <- match.arg(type)

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
        } else {
          ans <- matrix(rowSums2(x, cols = cols), ncol = 1)
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
