### =============================================================================
### rowsum
###

# TODO: Add `rows` and `cols` args?
.DelayedMatrix_block_rowsum <- function(x, group, reorder = TRUE, ...) {
  # Check input
  stopifnot(is(x, "DelayedMatrix"))
  DelayedArray:::.get_ans_type(x, must.be.numeric = TRUE)

  # Compute result
  val <- rowblock_APPLY(x = x,
                        APPLY = base::rowsum.default,
                        group = group,
                        reorder = reorder,
                        ...)
  if (length(val) == 0L) {
    return(numeric(nrow(x)))
  }
  do.call("rbind", val)
}

### ----------------------------------------------------------------------------
### Exported methods
###

# ------------------------------------------------------------------------------
# General method
#

#' @importMethodsFrom DelayedArray seed
#' @rdname colsum
#' @export
#' @examples
#'
#' rowsum(dm_HDF5, group = c(1, 1, 1, 2, 2))
setMethod("rowsum", "DelayedMatrix",
          function(x, group, reorder = TRUE, force_block_processing = FALSE,
                   ...) {
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
                              force_block_processing = TRUE,
                              ...))
              }
            }

            rowsum(x = simple_seed_x,
                   group = group,
                   reorder = reorder,
                   ...)
          }
)

# ------------------------------------------------------------------------------
# Seed-aware methods
#

#' @rdname colsum
#' @export
setMethod("rowsum", "matrix", base::rowsum)

#' @rdname colsum
#' @export
setMethod(
  "rowsum",
  "HDF5Matrix",
  function(x, group, reorder = TRUE, filepath = NULL, name = NULL,
           chunkdim = NULL, level = NULL, type = c("double", "integer"),
           BPPARAM = bpparam()) {

    # Check arguments ----------------------------------------------------------

    if (!type(x) %in% c("integer", "double")) {
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
    # TODO: Default is type = "double" because colSums2() returns numeric, but
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

    sink_grid <- RegularArrayGrid(refdim = dim(sink), spacings = c(1L, ncol(x)))
    list_of_rows <- split(seq_along(group), group)[as.character(ugroup)]

    # Compute colsum() ---------------------------------------------------------

    bplapply(
      X = seq_along(sink_grid),
      FUN = function(b, x, sink, sink_lock, sink_grid, list_of_rows) {
        rows <- list_of_rows[[b]]
        if (length(rows) == 1L) {
          ans <- as.matrix(x[rows, , drop = FALSE])
        } else {
          ans <- matrix(colSums2(x, rows = rows), nrow = 1)
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
