### =============================================================================
### Utility functions that probably belong in DelayedArray package
###

# NOTE: Adapted from DelayedArray:::block_APPLY(), adds the `MARGIN` argument
#' @importFrom DelayedArray RegularArrayGrid
#' @importMethodsFrom DelayedArray type write_block_to_sink
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
  get_spacings_for_capped_length_blocks <-
    DelayedArray:::get_spacings_for_capped_length_blocks
  if (MARGIN == 1L) {
    spacings <- rev(get_spacings_for_capped_length_blocks(rev(dim(x)),
                                                          max_block_len,
                                                          block_shape="linear"))
  } else if (MARGIN == 2L) {
    spacings <- get_spacings_for_capped_length_blocks(dim(x),
                                                      max_block_len,
                                                      block_shape="linear")
  } else {
    stop("'MARGIN' must be 1L or 2L")
  }
  grid <- RegularArrayGrid(dim(x), spacings)
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
#' @importMethodsFrom DelayedArray type
#' @keywords internal
#' @return A list of length equal to `ncol(x)`, each list element storing the
#' result for the corresponding column.
rowblock_APPLY <- function(x, APPLY, ..., sink = NULL) {
  x_dim <- dim(x)
  if (length(x_dim) != 2L) {
    stop("'x' must be a matrix-like object")
  }
  APPLY <- match.fun(APPLY)
  max_block_len <- max(DelayedArray:::get_max_block_length(type(x)),
                       x_dim[[2L]])
  block_APPLY(x, APPLY, MARGIN = 1, ..., sink = sink,
              max_block_len = max_block_len)
}
