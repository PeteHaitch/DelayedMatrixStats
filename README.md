
<!-- README.md is generated from README.Rmd. Please edit that file -->
DelayedMatrixStats
==================

**DelayedMatrixStats** is a port of the [**matrixStats**](https://CRAN.R-project.org/package=matrixStats) API to work with *DelayedMatrix* objects from the [**DelayedArray**](http://bioconductor.org/packages/DelayedArray/) package.

For a *DelayedMatrix*, `x`, the simplest way to apply a function, `f()`, from **matrixStats** is`matrixStats::f(as.matrix(x))`. However, this "*realizes*" `x` in memory as a *base::matrix*, which typically defeats the entire purpose of using a *DelayedMatrix* for storing the data.

The **DelayedArray** package already implements a clever strategy called "block-processing" for certain common "matrix stats" operations (e.g. `colSums()`, `rowSums()`). This is a good start, but not all of the **matrixStats** API is currently supported. Furthermore, certain operations can be optimized with additional information about `x`. For example, if `x` is an *RleArray*, then `colSums(x)` can be very efficiently implemented by calling `sum,Rle-method()` on `x[, j]` for each `j`.

Installation
------------

You can install DelayedMatrixStats from github with:

``` r
# install.packages("devtools")
devtools::install_github("PeteHaitch/DelayedMatrixStats")
```

Example
-------

This is a basic example which shows you how to solve a common problem:

``` r
## TODO
```

API coverage
------------

-   \[ \] `colAlls()`
-   \[ \] `colAnyMissings()`
-   \[ \] `colAnyNAs()`
-   \[ \] `colAnys()`
-   \[ \] `colAvgsPerRowSet()`
-   \[ \] `colCollapse()`
-   \[ \] `colCounts()`
-   \[ \] `colCummaxs()`
-   \[ \] `colCummins()`
-   \[ \] `colCumprods()`
-   \[ \] `colCumsums()`
-   \[ \] `colDiffs()`
-   \[ \] `colIQRDiffs()`
-   \[ \] `colIQRs()`
-   \[ \] `colLogSumExps()`
-   \[ \] `colMadDiffs()`
-   \[ \] `colMads()`
-   \[ \] `colMaxs()`
-   \[ \] `colMeans2()`
-   \[ \] `colMedians()`
-   \[ \] `colMins()`
-   \[ \] `colOrderStats()`
-   \[ \] `colProds()`
-   \[ \] `colQuantiles()`
-   \[ \] `colRanges()`
-   \[ \] `colRanks()`
-   \[ \] `colSdDiffs()`
-   \[ \] `colSds()`
-   \[ \] `colSums2()`
-   \[ \] `colTabulates()`
-   \[ \] `colVarDiffs()`
-   \[ \] `colVars()`
-   \[ \] `colWeightedMads()`
-   \[ \] `colWeightedMeans()`
-   \[ \] `colWeightedMedians()`
-   \[ \] `colWeightedSds()`
-   \[ \] `colWeightedVars()`
-   \[ \] `rowAlls()`
-   \[ \] `rowAnyMissings()`
-   \[ \] `rowAnyNAs()`
-   \[ \] `rowAnys()`
-   \[ \] `rowAvgsPerColSet()`
-   \[ \] `rowCollapse()`
-   \[ \] `rowCounts()`
-   \[ \] `rowCummaxs()`
-   \[ \] `rowCummins()`
-   \[ \] `rowCumprods()`
-   \[ \] `rowCumsums()`
-   \[ \] `rowDiffs()`
-   \[ \] `rowIQRDiffs()`
-   \[ \] `rowIQRs()`
-   \[ \] `rowLogSumExps()`
-   \[ \] `rowMadDiffs()`
-   \[ \] `rowMads()`
-   \[ \] `rowMaxs()`
-   \[ \] `rowMeans2()`
-   \[ \] `rowMedians()`
-   \[ \] `rowMins()`
-   \[ \] `rowOrderStats()`
-   \[ \] `rowProds()`
-   \[ \] `rowQuantiles()`
-   \[ \] `rowRanges()`
-   \[ \] `rowRanks()`
-   \[ \] `rowSdDiffs()`
-   \[ \] `rowSds()`
-   \[x\] `rowSums2()`
-   \[ \] `rowTabulates()`
-   \[ \] `rowVarDiffs()`
-   \[ \] `rowVars()`
-   \[ \] `rowWeightedMads()`
-   \[ \] `rowWeightedMeans()`
-   \[ \] `rowWeightedMedians()`
-   \[ \] `rowWeightedSds()`
-   \[ \] `rowWeightedVars()`
