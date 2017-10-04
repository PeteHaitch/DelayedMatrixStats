#' @examples
#' # A DelayedMatrix with a 'DataFrame' seed
#' dm_DF <- DelayedArray(S4Vectors::DataFrame(C1 = rep(1L, 5),
#'                                            C2 = as.integer((0:4) ^ 2),
#'                                            C3 = seq(-5L, -1L, 1L)))
