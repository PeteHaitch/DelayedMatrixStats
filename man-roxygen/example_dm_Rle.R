#' @examples
#' # A DelayedMatrix with a 'SolidRleArraySeed' seed
#' dm_Rle <- RleArray(Rle(c(rep(1, 5), 0:4)), dim = c(5, 2))
#' class(seed(dm_Rle))
