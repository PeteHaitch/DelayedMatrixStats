#' DelayedMatrixStats: Functions that apply to rows and columns of
#' *DelayedMatrix* objects.
#'
#' **DelayedMatrixStats** is a port of the
#' [**matrixStats**](https://CRAN.R-project.org/package=matrixStats) API to
#' work with *DelayedMatrix* objects from the
#' [**DelayedArray**](http://bioconductor.org/packages/DelayedArray/)
#' package. High-performing functions operating on rows and columns of
#' *DelayedMatrix* objects, e.g. `colMedians()` / `rowMedians()`,
#' `colRanks()` / `rowRanks()`, and `colSds()` / `rowSds()`. Functions
#' optimized per data type and for subsetted calculations such that both memory
#' usage and processing time is minimized.
#'
#' @name DelayedMatrixStats
#' @docType package
NULL
