#' @examples
#' # A DelayedMatrix with a 'HDF5ArraySeed' seed
#' library(HDF5Array)
#' dm_HDF5 <- writeHDF5Array(matrix(c(rep(1, 5), 1:5), ncol = 2))
#' class(seed(dm_HDF5))
