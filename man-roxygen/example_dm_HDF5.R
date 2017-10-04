#' @examples
#' # A DelayedMatrix with a 'HDF5ArraySeed' seed
#' # NOTE: Requires that the HDF5Array package is installed
#' library(HDF5Array)
#' dm_HDF5 <- writeHDF5Array(matrix(c(rep(1L, 5),
#'                                    as.integer((0:4) ^ 2),
#'                                    seq(-5L, -1L, 1L)),
#'                                  ncol = 3))
