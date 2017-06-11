
<!-- README.md is generated from README.Rmd. Please edit that file -->
DelayedMatrixStats
==================

[![Travis-CI Build Status](https://travis-ci.org/PeteHaitch/DelayedMatrixStats.svg?branch=master)](https://travis-ci.org/PeteHaitch/DelayedMatrixStats) [![Coverage Status](https://img.shields.io/codecov/c/github/PeteHaitch/DelayedMatrixStats/master.svg)](https://codecov.io/github/PeteHaitch/DelayedMatrixStats?branch=master)

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
library(profmem)

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
#>                                        expr        min         lq
#>         DelayedArray::colSums(dense_matrix) 4034.08048 4371.03038
#>  DelayedMatrixStats::colSums2(dense_matrix)   13.41289   13.77259
#>        mean   median         uq        max neval
#>  5003.47465 4720.567 5153.30394 7160.62407    10
#>    19.38687   15.805   25.92883   34.93857    10
profmem::total(profmem::profmem(DelayedArray::colSums(dense_matrix)))
#> [1] 2498170032
profmem::total(profmem::profmem(DelayedMatrixStats::colSums2(dense_matrix)))
#> [1] 175424

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
#>                                         expr        min         lq
#>         DelayedArray::colSums(sparse_matrix) 1040.88851 1208.67881
#>  DelayedMatrixStats::colSums2(sparse_matrix)   11.08855   11.34014
#>        mean     median         uq        max neval
#>  1308.20718 1292.34716 1410.13812 1672.21904    10
#>    13.12051   12.24028   12.73219   18.71125    10
profmem::total(profmem::profmem(DelayedArray::colSums(sparse_matrix)))
#> [1] 1709266424
profmem::total(profmem::profmem(DelayedMatrixStats::colSums2(sparse_matrix)))
#> [1] 15376

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
#>         DelayedArray::colSums(rle_matrix) 1315.479149 1373.755265
#>  DelayedMatrixStats::colSums2(rle_matrix)    4.256474    4.350228
#>       mean      median         uq        max neval
#>  1610.5489 1452.909270 1893.17503 2109.70966    10
#>    13.4017    8.935661   12.74955   59.15958    10
profmem::total(profmem::profmem(DelayedArray::colSums(rle_matrix)))
#> [1] 594926952
profmem::total(profmem::profmem(DelayedMatrixStats::colSums2(rle_matrix)))
#> [1] 11784
```

Benchmarking
------------

An extensive set of benchmarks is under development at <http://peterhickey.org/BenchmarkingDelayedMatrixStats/>.

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
-   \[x\] `colMedians()`
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
