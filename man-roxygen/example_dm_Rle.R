#' @examples
#' # A DelayedMatrix with a 'SolidRleArraySeed' seed
#' dm_Rle <- RleArray(Rle(c(rep(1L, 5),
#'                          as.integer((0:4) ^ 2),
#'                          seq(-5L, -1L, 1L))),
#'                    dim = c(5, 3))
