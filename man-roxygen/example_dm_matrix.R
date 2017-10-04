#' @examples
#' # A DelayedMatrix with a 'matrix' seed
#' dm_matrix <- DelayedArray(matrix(c(rep(1L, 5),
#'                                    as.integer((0:4) ^ 2),
#'                                    seq(-5L, -1L, 1L)),
#'                                  ncol = 3))
