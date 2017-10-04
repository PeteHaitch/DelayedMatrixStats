#' @examples
#' # A DelayedMatrix with a 'DataFrame' seed
#' dm_DF <- DelayedArray(S4Vectors::DataFrame(C1 = rep(1, 5), C2 = 1:5))
#' class(seed(dm_DF))
