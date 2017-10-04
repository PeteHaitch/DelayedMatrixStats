#' @examples
#' # A DelayedMatrix with a 'Matrix' seed
#' dm_Matrix <- DelayedArray(Matrix::Matrix(c(rep(1, 5), 1:5), ncol = 2))
#' class(seed(dm_Matrix))
