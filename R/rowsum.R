### =============================================================================
### rowsum
###

#' @importFrom BiocParallel bplapply bpparam ipcid ipclock ipcremove ipcunlock
#' @importFrom DelayedArray rowsum rowGrid
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
