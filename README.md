
<!-- README.md is generated from README.Rmd. Please edit that file -->
DelayedMatrixStats
==================

[![Travis-CI Build Status](https://travis-ci.org/PeteHaitch/DelayedMatrixStats.svg?branch=master)](https://travis-ci.org/PeteHaitch/DelayedMatrixStats) [![Coverage Status](https://codecov.io/gh/PeteHaitch/DelayedMatrixStats/branch/master/graph/badge.svg)](https://codecov.io/gh/PeteHaitch/DelayedMatrixStats)

**DelayedMatrixStats** is a port of the [**matrixStats**](https://CRAN.R-project.org/package=matrixStats) API to work with *DelayedMatrix* objects from the [**DelayedArray**](http://bioconductor.org/packages/DelayedArray/) package.

For a *DelayedMatrix*, `x`, the simplest way to apply a function, `f()`, from **matrixStats** is`matrixStats::f(as.matrix(x))`. However, this "*realizes*" `x` in memory as a *base::matrix*, which typically defeats the entire purpose of using a *DelayedMatrix* for storing the data.

The **DelayedArray** package already implements a clever strategy called "block-processing" for certain common "matrix stats" operations (e.g. `colSums()`, `rowSums()`). This is a good start, but not all of the **matrixStats** API is currently supported. Furthermore, certain operations can be optimized with additional information about `x`. I'll refer to these "seed-aware" implementations.

Installation
------------

You can install **DelayedMatrixStats** from github with:

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
library(profmem)
```

``` r
set.seed(666)

# Fast column sums of DelayedMatrix with matrix seed
dense_matrix <- DelayedArray(matrix(runif(20000 * 600), nrow = 20000,
                                    ncol = 600))
class(seed(dense_matrix))
#> [1] "matrix"
dense_matrix
#> DelayedMatrix object of 20000 x 600 doubles:
#>                [,1]       [,2]       [,3] ...     [,599]     [,600]
#>     [1,]  0.7743685  0.6601787  0.4098798   . 0.89118118 0.05776471
#>     [2,]  0.1972242  0.8436035  0.9198450   . 0.31799523 0.63099417
#>     [3,]  0.9780138  0.2017589  0.4696158   . 0.31783791 0.02830454
#>     [4,]  0.2013274  0.8797239  0.6474768   . 0.55217184 0.09678816
#>     [5,]  0.3612444  0.8158778  0.5928599   . 0.08530977 0.39224147
#>      ...          .          .          .   .          .          .
#> [19996,] 0.19490291 0.07763570 0.56391725   . 0.09703424 0.62659353
#> [19997,] 0.61182993 0.01910121 0.04046034   . 0.59708388 0.88389731
#> [19998,] 0.12932744 0.21155070 0.19344085   . 0.51682032 0.13378223
#> [19999,] 0.18985573 0.41716539 0.35110782   . 0.62939661 0.94601427
#> [20000,] 0.87889047 0.25308041 0.54666920   . 0.81630322 0.73272217
microbenchmark(DelayedArray::colSums(dense_matrix),
               DelayedMatrixStats::colSums2(dense_matrix),
               times = 10)
#> Unit: milliseconds
#>                                        expr      min        lq      mean
#>         DelayedArray::colSums(dense_matrix) 329.2488 348.55194 382.94905
#>  DelayedMatrixStats::colSums2(dense_matrix)  14.1121  16.42888  19.10789
#>    median        uq       max neval
#>  372.0184 409.98222 461.98058    10
#>   17.9118  20.79366  29.14484    10
profmem::total(profmem::profmem(DelayedArray::colSums(dense_matrix)))
#> [1] 386181032
profmem::total(profmem::profmem(DelayedMatrixStats::colSums2(dense_matrix)))
#> [1] 172544

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
#>               [,1]      [,2]      [,3] ...     [,599]     [,600]
#>     [1,] 0.7743685 0.0000000 0.4098798   .  0.8911812  0.0000000
#>     [2,] 0.0000000 0.0000000 0.9198450   .  0.3179952  0.6309942
#>     [3,] 0.9780138 0.0000000 0.4696158   .  0.0000000  0.0000000
#>     [4,] 0.0000000 0.8797239 0.0000000   .  0.0000000  0.0000000
#>     [5,] 0.0000000 0.0000000 0.5928599   .  0.0000000  0.3922415
#>      ...         .         .         .   .          .          .
#> [19996,] 0.1949029 0.0000000 0.5639173   . 0.09703424 0.62659353
#> [19997,] 0.6118299 0.0000000 0.0000000   . 0.00000000 0.88389731
#> [19998,] 0.0000000 0.0000000 0.1934408   . 0.51682032 0.00000000
#> [19999,] 0.0000000 0.0000000 0.0000000   . 0.62939661 0.94601427
#> [20000,] 0.8788905 0.0000000 0.0000000   . 0.81630322 0.00000000
microbenchmark(DelayedArray::colSums(sparse_matrix),
               DelayedMatrixStats::colSums2(sparse_matrix),
               times = 10)
#> Unit: milliseconds
#>                                         expr       min        lq      mean
#>         DelayedArray::colSums(sparse_matrix) 565.25301 586.58518 621.49343
#>  DelayedMatrixStats::colSums2(sparse_matrix)  11.85024  12.15978  14.69348
#>     median        uq       max neval
#>  625.99292 649.16133 675.30484    10
#>   14.88927  15.70162  21.21839    10
profmem::total(profmem::profmem(DelayedArray::colSums(sparse_matrix)))
#> [1] 442022688
profmem::total(profmem::profmem(DelayedMatrixStats::colSums2(sparse_matrix)))
#> [1] 16168

# Fast column sums of DelayedMatrix with Rle-based seed
rle_matrix <- RleArray(Rle(sample(2L, 200000 * 6 / 10, replace = TRUE), 100),
                       dim = c(2000000, 6))
class(seed(rle_matrix))
#> [1] "SolidRleArraySeed"
#> attr(,"package")
#> [1] "DelayedArray"
rle_matrix
#> RleMatrix object of 2000000 x 6 integers:
#>            [,1] [,2] [,3] [,4] [,5] [,6]
#>       [1,]    2    2    2    1    2    1
#>       [2,]    2    2    2    1    2    1
#>       [3,]    2    2    2    1    2    1
#>       [4,]    2    2    2    1    2    1
#>       [5,]    2    2    2    1    2    1
#>        ...    .    .    .    .    .    .
#> [1999996,]    2    2    1    1    2    1
#> [1999997,]    2    2    1    1    2    1
#> [1999998,]    2    2    1    1    2    1
#> [1999999,]    2    2    1    1    2    1
#> [2000000,]    2    2    1    1    2    1
microbenchmark(DelayedArray::colSums(rle_matrix),
               DelayedMatrixStats::colSums2(rle_matrix),
               times = 10)
#> Unit: milliseconds
#>                                      expr         min          lq
#>         DelayedArray::colSums(rle_matrix) 1235.254924 1249.930752
#>  DelayedMatrixStats::colSums2(rle_matrix)    4.384281    4.730135
#>        mean      median          uq        max neval
#>  1309.14233 1283.126403 1350.572835 1453.54344    10
#>    14.14867    7.702521    8.612942   75.81422    10
profmem::total(profmem::profmem(DelayedArray::colSums(rle_matrix)))
#> [1] 592057872
profmem::total(profmem::profmem(DelayedMatrixStats::colSums2(rle_matrix)))
#> [1] 12576
```

Benchmarking
------------

An extensive set of benchmarks is under development at <http://peterhickey.org/BenchmarkingDelayedMatrixStats/>.

API coverage
------------

| Method                 | Block processing | *base::matrix* optimized | *Matrix::Matrix* optimized | *DelayedArray::RleArray* (*SolidRleArraySeed*) optimized | *DelayedArray::RleArray* (*ChunkedRleArraySeed*) optimized | *HDF5Array::HDF5Matrix* optimized | *base::data.frame* optimized | *S4Vectors::DataFrame* optimized |
|:-----------------------|:-----------------|:-------------------------|:---------------------------|:---------------------------------------------------------|:-----------------------------------------------------------|:----------------------------------|:-----------------------------|:---------------------------------|
| `colAlls()`            | ✔️               | ✔️                       | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `colAnyMissings()`     | ✔️               | ✔️                       | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `colAnyNAs()`          | ✔️               | ✔️                       | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `colAnys()`            | ✔️               | ✔️                       | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `colAvgsPerRowSet()`   | ❌                | ❌                        | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `colCollapse()`        | ❌                | ❌                        | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `colCounts()`          | ✔️               | ✔️                       | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `colCummaxs()`         | ✔️               | ✔️                       | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `colCummins()`         | ✔️               | ✔️                       | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `colCumprods()`        | ✔️               | ✔️                       | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `colCumsums()`         | ✔️               | ✔️                       | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `colDiffs()`           | ✔️               | ✔️                       | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `colIQRDiffs()`        | ✔️               | ✔️                       | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `colIQRs()`            | ✔️               | ✔️                       | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `colLogSumExps()`      | ✔️               | ✔️                       | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `colMadDiffs()`        | ✔️               | ✔️                       | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `colMads()`            | ✔️               | ✔️                       | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `colMaxs()`            | ❌                | ✔️                       | ✔️                         | ✔️                                                       | ✔️                                                         | ✔️                                | ✔️                           | ✔️                               |
| `colMeans2()`          | ✔️               | ✔️                       | ✔️                         | ✔️                                                       | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `colMedians()`         | ✔️               | ✔️                       | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `colMins()`            | ❌                | ✔️                       | ✔️                         | ✔️                                                       | ✔️                                                         | ✔️                                | ✔️                           | ✔️                               |
| `colOrderStats()`      | ✔️               | ✔️                       | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `colProds()`           | ✔️               | ✔️                       | ❌                          | ✔️                                                       | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `colQuantiles()`       | ✔️               | ✔️                       | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `colRanges()`          | ❌                | ✔️                       | ✔️                         | ✔️                                                       | ✔️                                                         | ✔️                                | ✔️                           | ✔️                               |
| `colRanks()`           | ✔️               | ✔️                       | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `colSdDiffs()`         | ✔️               | ✔️                       | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `colSds()`             | ✔️               | ✔️                       | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `colSums2()`           | ✔️               | ✔️                       | ✔️                         | ✔️                                                       | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `colTabulates()`       | ❌                | ❌                        | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `colVarDiffs()`        | ✔️               | ✔️                       | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `colVars()`            | ✔️               | ✔️                       | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `colWeightedMads()`    | ✔️               | ✔️                       | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `colWeightedMeans()`   | ✔️               | ✔️                       | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `colWeightedMedians()` | ✔️               | ✔️                       | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `colWeightedSds()`     | ✔️               | ✔️                       | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `colWeightedVars()`    | ✔️               | ✔️                       | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `rowAlls()`            | ✔️               | ✔️                       | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `rowAnyMissings()`     | ✔️               | ✔️                       | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `rowAnyNAs()`          | ✔️               | ✔️                       | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `rowAnys()`            | ✔️               | ✔️                       | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `rowAvgsPerColSet()`   | ❌                | ❌                        | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `rowCollapse()`        | ❌                | ❌                        | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `rowCounts()`          | ✔️               | ✔️                       | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `rowCummaxs()`         | ✔️               | ✔️                       | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `rowCummins()`         | ✔️               | ✔️                       | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `rowCumprods()`        | ✔️               | ✔️                       | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `rowCumsums()`         | ✔️               | ✔️                       | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `rowDiffs()`           | ✔️               | ✔️                       | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `rowIQRDiffs()`        | ✔️               | ✔️                       | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `rowIQRs()`            | ❌                | ❌                        | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `rowLogSumExps()`      | ❌                | ❌                        | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `rowMadDiffs()`        | ❌                | ❌                        | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `rowMads()`            | ❌                | ❌                        | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `rowMaxs()`            | ❌                | ✔️                       | ✔️                         | ✔️                                                       | ✔️                                                         | ✔️                                | ✔️                           | ✔️                               |
| `rowMeans2()`          | ❌                | ❌                        | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `rowMedians()`         | ❌                | ❌                        | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `rowMins()`            | ❌                | ✔️                       | ✔️                         | ✔️                                                       | ✔️                                                         | ✔️                                | ✔️                           | ✔️                               |
| `rowOrderStats()`      | ❌                | ❌                        | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `rowProds()`           | ❌                | ❌                        | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `rowQuantiles()`       | ❌                | ❌                        | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `rowRanges()`          | ❌                | ✔️                       | ✔️                         | ✔️                                                       | ✔️                                                         | ✔️                                | ✔️                           | ✔️                               |
| `rowRanks()`           | ❌                | ❌                        | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `rowSdDiffs()`         | ❌                | ❌                        | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `rowSds()`             | ❌                | ❌                        | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `rowSums2()`           | ❌                | ❌                        | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `rowTabulates()`       | ❌                | ❌                        | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `rowVarDiffs()`        | ❌                | ❌                        | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `rowVars()`            | ❌                | ❌                        | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `rowWeightedMads()`    | ❌                | ❌                        | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `rowWeightedMeans()`   | ❌                | ❌                        | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `rowWeightedMedians()` | ❌                | ❌                        | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `rowWeightedSds()`     | ❌                | ❌                        | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
| `rowWeightedVars()`    | ❌                | ❌                        | ❌                          | ❌                                                        | ❌                                                          | ❌                                 | ❌                            | ❌                                |
