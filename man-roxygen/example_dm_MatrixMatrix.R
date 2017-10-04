#' @examples
#' # A DelayedMatrix with a 'Matrix' seed
#' dm_Matrix <- DelayedArray(Matrix::Matrix(c(rep(1L, 5),
#'                                            as.integer((0:4) ^ 2),
#'                                            seq(-5L, -1L, 1L)),
#'                                          ncol = 3))
