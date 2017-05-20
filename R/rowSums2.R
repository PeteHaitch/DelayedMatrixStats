#' @inherit matrixStats::rowSums2
#' @importFrom DelayedArray seed
#' @importFrom matrixStats rowSums2
#' @importFrom methods is
#' @export
rowSums2 <- function(x, rows = NULL, cols = NULL, na.rm = FALSE,
                     dim. = dim(x), ...) {
  if (is(x, "DelayedMatrix")) {
    seed <- seed(x)
    if (is.matrix(seed)) {
      # TODO: Is it more efficient to do x <- x[rows, cols] before passing to
      #       rowSums2()?
      # TODO: Basically no cost for calling as.matrix() on a DelayedMatrix with
      #       a matrix-seed; true? Need to realise delayed ops on seed before
      #       passing to rowSums2. Want to pass seed to
      #       matrixStats::rowSums2(), with rows and cols potentially subset by
      #       unname(x@index), and with delayed ops realized. Basically, want
      #       to do as.matrix,DelayedMatrix-method as efficiently as possible
      x <- as.matrix(x)
      return(matrixStats::rowSums2(x = x,
                                   rows = rows,
                                   cols = cols,
                                   na.rm = na.rm,
                                   dim. = dim(x),
                                   ...))
    } else if (is(seed, "Matrix")) {
      stop("Not yet implemented")
      if (!is.null(rows)) {
        x <- x[rows, ]
      }
      if (!is.null(cols)) {
        x <- x[, cols]
      }
      # TODO: Need to realise delayed ops on seed before passing to rowSums2.
      #       Want to pass seed to Matrix::rowSums(),
      #       with rows and cols potentially subset by unname(x@index), and
      #       with delayed ops realized. Basically, want to do
      #       as.matrix,DelayedMatrix-method as efficiently as possible
    } else {
      # NOTE: Fall back on default rowSums,DelayedArray-method if seed is
      #       complex (which makes optimization difficult)
      if (!is.null(rows)) {
        x <- x[rows, ]
      }
      if (!is.null(cols)) {
        x <- x[, cols]
      }
      DelayedArray::rowSums(x, na.rm = na.rm, dims = 1L)
    }
  } else if (is.matrix(x)) {
    matrixStats::rowSums2(x = x, rows = rows, cols = cols, na.rm = na.rm,
                          dim. = dim(x), ...)
  }
}

# TODO: Unit tests for correctness
# TODO: Benchmarks for performance