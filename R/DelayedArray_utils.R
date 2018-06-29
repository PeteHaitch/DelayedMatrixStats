### ============================================================================
### Utility functions that probably belong in DelayedArray package
###

#' Get the class of the seed of a DelayedArray
#'
#' @note Can't use `class(seed(x))` because `seed(x)` will return an error if
#' the seed is a [DelayedArray::DelayedOp-class].
#' @param x A [DelayedArray::DelayedArray-class].
#' @return The class of the seed of the [DelayedArray::DelayedArray-class]
#' object.
#' @keywords internal
seedClass <- function(x) {
  class(x@seed)
}

# NOTE: Adapted from DelayedArray:::block_APPLY(), adds the `MARGIN` argument
#' @importFrom DelayedArray makeCappedVolumeBox RegularArrayGrid
#' @importMethodsFrom DelayedArray type read_block write_block
#' @importFrom S4Vectors isSingleNumber
#' @keywords internal
block_APPLY <- function(x, APPLY, MARGIN, ..., sink = NULL,
                        max_block_len = NULL) {
  APPLY <- match.fun(APPLY)
  if (is.null(max_block_len)) {
    max_block_len <- DelayedArray:::get_default_block_maxlength(type(x))
  }
  if (!isSingleNumber(MARGIN)) {
    stop("'MARGIN' must be a single integer")
  }
  if (!is.integer(MARGIN)) {
    MARGIN <- as.integer(MARGIN)
  }
  if (MARGIN == 1L) {
    spacings <- rev(makeCappedVolumeBox(max_block_len, rev(dim(x)), "linear"))
  } else if (MARGIN == 2L) {
    spacings <- makeCappedVolumeBox(max_block_len, dim(x), "linear")
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
    block <- read_block(x, viewport)
    block_ans <- APPLY(block, ...)
    if (!is.null(sink)) {
      write_block(sink, viewport, block_ans)
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
  max_block_len <- max(DelayedArray:::get_default_block_maxlength(type(x)),
                       x_dim[[2L]])
  block_APPLY(x, APPLY, MARGIN = 1, ..., sink = sink,
              max_block_len = max_block_len)
}
