
<!-- README.md is generated from README.Rmd. Please edit that file -->
DelayedMatrixStats
==================

**DelayedMatrixStats** is a port of the [**matrixStats**](https://CRAN.R-project.org/package=matrixStats) API to work with *DelayedMatrix* objects from the [**DelayedArray**](http://bioconductor.org/packages/DelayedArray/) package.

For a *DelayedMatrix*, `x`, the simplest way to apply a function, `f()`, from **matrixStats** is`matrixStats::f(as.matrix(x))`. However, this "*realizes*" `x` in memory as a *base::matrix*, which typically defeats the entire purpose of using a *DelayedMatrix* for storing the data.

The **DelayedArray** package already implements a clever strategy called "block-processing" for certain common "matrix stats" operations (e.g. `colSums()`, `rowSums()`). This is a good start, but not all of the **matrixStats** API is currently supported. Furthermore, certain operations can be optimized with additional information about `x`. I'll refer to these "seed-aware" implementations.

Installation
------------

You can install DelayedMatrixStats from github with:

``` r
# install.packages("devtools")
devtools::install_github("PeteHaitch/DelayedMatrixStats")
```

Example
-------

This example compares two ways of computing column sums of a *DelayedMatrix* object:

1.  `DelayedMatrix::colSums()`: The 'block-processing strategy', implemented in the **DelayedArray** package. The block-processing strategy works for any *DelayedMatrix* object, regardless of the type of *seed*.
2.  `DelayedMatrixStats::colSums2()`: The 'seed-aware' strategy, implemented in the **DelayedMatrixStats** package. The seed-aware implementation is optimized for both speed and memory but only for *DelayedMatrix* objects with certain types of *seed*.

``` r
library(DelayedMatrixStats)
library(Matrix)
library(microbenchmark)

# Fast column sums of DelayedMatrix with matrix seed
dense_matrix <- DelayedArray(matrix(runif(20000 * 600), nrow = 20000,
                                    ncol = 600))
class(seed(dense_matrix))
#> [1] "matrix"
dense_matrix
#> DelayedMatrix object of 20000 x 600 doubles:
#>                 [,1]        [,2]        [,3]      .     [,599]     [,600]
#>     [1,] 0.398746183 0.658995559 0.346733093      . 0.46913117 0.10999856
#>     [2,] 0.944626837 0.008473052 0.600675274      . 0.99329021 0.02908414
#>     [3,] 0.123061080 0.056541906 0.444670161      . 0.69550733 0.98232569
#>     [4,] 0.201957127 0.548035120 0.827493285      . 0.49562932 0.61086330
#>     [5,] 0.737335345 0.839235505 0.535531887      . 0.86757552 0.05317334
#>      ...           .           .           .      .          .          .
#> [19996,]  0.75109176  0.12324372  0.99137732      .  0.6408046  0.7691766
#> [19997,]  0.22065391  0.91954252  0.02550806      .  0.9688482  0.8275454
#> [19998,]  0.86562732  0.81484728  0.03914879      .  0.3328586  0.7085412
#> [19999,]  0.79949787  0.83112454  0.45859493      .  0.8742511  0.1713129
#> [20000,]  0.70050093  0.96788879  0.19596691      .  0.9052799  0.0220140
microbenchmark(DelayedArray::colSums(dense_matrix),
               DelayedMatrixStats::colSums2(dense_matrix),
               times = 10)
#> Unit: milliseconds
#>                                        expr        min         lq
#>         DelayedArray::colSums(dense_matrix) 4462.00977 4652.81862
#>  DelayedMatrixStats::colSums2(dense_matrix)   13.26318   14.29185
#>        mean     median         uq        max neval
#>  5369.90054 5445.60099 6139.99734 6227.20492    10
#>    18.46018   17.34971   18.58638   28.38708    10

# Fast, low-memory column sums of DelayedMatrix with sparse matrix seed
sparse_matrix <- seed(dense_matrix)
zero_idx <- sample(length(sparse_matrix), 0.6 * length(sparse_matrix))
sparse_matrix[zero_idx] <- 0
sparse_matrix <- DelayedArray(Matrix::Matrix(sparse_matrix, sparse = TRUE))
class(seed(sparse_matrix))
#> [1] "dgCMatrix"
#> attr(,"package")
#> [1] "Matrix"
sparse_matrix
#> DelayedMatrix object of 20000 x 600 doubles:
#>                [,1]       [,2]       [,3]      .    [,599]    [,600]
#>     [1,] 0.00000000 0.65899556 0.00000000      . 0.0000000 0.1099986
#>     [2,] 0.00000000 0.00000000 0.00000000      . 0.0000000 0.0000000
#>     [3,] 0.12306108 0.05654191 0.00000000      . 0.0000000 0.0000000
#>     [4,] 0.00000000 0.54803512 0.00000000      . 0.4956293 0.0000000
#>     [5,] 0.00000000 0.00000000 0.53553189      . 0.0000000 0.0000000
#>      ...          .          .          .      .         .         .
#> [19996,] 0.75109176 0.00000000 0.00000000      . 0.6408046 0.0000000
#> [19997,] 0.00000000 0.00000000 0.00000000      . 0.9688482 0.0000000
#> [19998,] 0.00000000 0.81484728 0.03914879      . 0.0000000 0.7085412
#> [19999,] 0.00000000 0.83112454 0.00000000      . 0.0000000 0.1713129
#> [20000,] 0.00000000 0.96788879 0.19596691      . 0.9052799 0.0220140
microbenchmark(DelayedArray::colSums(sparse_matrix),
               DelayedMatrixStats::colSums2(sparse_matrix),
               times = 10)
#> Unit: milliseconds
#>                                         expr        min         lq
#>         DelayedArray::colSums(sparse_matrix) 1095.97576 1109.95018
#>  DelayedMatrixStats::colSums2(sparse_matrix)   11.13022   11.17825
#>        mean     median         uq       max neval
#>  1216.33666 1229.67348 1272.99977 1424.2127    10
#>    12.13492   11.69185   12.29795   14.5534    10

# Fast column sums of DelayedMatrix with Rle-based seed
rle_matrix <- RleArray(Rle(sample(2L, 200000 * 6 / 10, replace = TRUE), 100),
                       dim = c(2000000, 6))
class(seed(rle_matrix))
#> [1] "RleArraySeed"
#> attr(,"package")
#> [1] "DelayedArray"
rle_matrix
#> RleMatrix object of 2000000 x 6 integers:
#>            [,1] [,2] [,3] [,4] [,5] [,6]
#>       [1,]    1    1    1    2    2    2
#>       [2,]    1    1    1    2    2    2
#>       [3,]    1    1    1    2    2    2
#>       [4,]    1    1    1    2    2    2
#>       [5,]    1    1    1    2    2    2
#>        ...    .    .    .    .    .    .
#> [1999996,]    2    2    2    2    1    1
#> [1999997,]    2    2    2    2    1    1
#> [1999998,]    2    2    2    2    1    1
#> [1999999,]    2    2    2    2    1    1
#> [2000000,]    2    2    2    2    1    1
microbenchmark(DelayedArray::colSums(rle_matrix),
               DelayedMatrixStats::colSums2(rle_matrix),
               times = 10)
#> Unit: milliseconds
#>                                      expr         min          lq
#>         DelayedArray::colSums(rle_matrix) 2607.380183 2638.167234
#>  DelayedMatrixStats::colSums2(rle_matrix)    3.771215    3.897232
#>        mean      median          uq        max neval
#>  2700.45370 2667.519814 2746.171231 2891.17710    10
#>    10.79183    4.316823    5.351091   61.17693    10
```

The [benchmarking vignette](vignettes/benchmarking.Rmd) includes further examples.

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
-   \[x\] `colSums2()`
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
