### =============================================================================
### Utility functions that probably belong in DelayedArray package
###

# NOTE: Adapted from DelayedArray:::block_APPLY()
#' @importFrom DelayedArray ArrayRegularGrid type write_block_to_sink
#' @importFrom S4Vectors isSingleNumber
#' @keywords internal
block_APPLY <- function(x, APPLY, MARGIN, ..., sink = NULL,
                        max_block_len = NULL) {
  APPLY <- match.fun(APPLY)
  if (is.null(max_block_len)) {
    max_block_len <- DelayedArray:::get_max_block_length(type(x))
  }
  if (!isSingleNumber(MARGIN)) {
    stop("'MARGIN' must be a single integer")
  }
  if (!is.integer(MARGIN)) {
    MARGIN <- as.integer(MARGIN)
  }
  if (MARGIN == 1L) {
    spacings <- DelayedArray:::get_max_spacings_for_linear_blocks(rev(dim(x)),
                                                                  max_block_len)
    grid <- ArrayRegularGrid(dim(x), rev(spacings))
  } else if (MARGIN == 2L) {
    spacings <- DelayedArray:::get_max_spacings_for_linear_blocks(dim(x),
                                                                  max_block_len)
    grid <- ArrayRegularGrid(dim(x), spacings)
  } else {
    stop("'MARGIN' must be 1L or 2L")
  }
  nblock <- length(grid)
  lapply(seq_len(nblock), function(b) {
    if (DelayedArray:::get_verbose_block_processing()) {
      message("Processing block ", b, "/", nblock, " ... ",
              appendLF = FALSE)
    }
    viewport <- grid[[b]]
    block <- DelayedArray:::extract_block(x, viewport)
    if (!is.array(block)) {
      block <- DelayedArray:::.as_array_or_matrix(block)
    }
    block_ans <- APPLY(block, ...)
    if (!is.null(sink)) {
      write_block_to_sink(block_ans, sink, viewport)
      block_ans <- NULL
    }
    if (DelayedArray:::get_verbose_block_processing()) {
      message("OK")
    }
    block_ans
  })
}

#' Adapted from `DelayedArray:::colblock_APPLY()`
#' @importFrom DelayedArray type
#' @keywords internal
rowblock_APPLY <- function(x, APPLY, ..., sink = NULL) {
  x_dim <- dim(x)
  if (length(x_dim) != 2L) {
    stop("'x' must be a matrix-like object")
  }
  APPLY <- match.fun(APPLY)
  max_block_len <- max(DelayedArray:::get_max_block_length(type(x)),
                       x_dim[[2L]])
  # TODO: How to apply over rows?
  # DelayedArray:::block_APPLY(x, APPLY, ..., sink = sink,
  #                            max_block_len = max_block_len)
  block_APPLY(x, APPLY, MARGIN = 2, ..., sink = sink,
              max_block_len = max_block_len)
}
