
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
library(profmem)

# Fast column sums of DelayedMatrix with matrix seed
dense_matrix <- DelayedArray(matrix(runif(20000 * 600), nrow = 20000,
                                    ncol = 600))
class(seed(dense_matrix))
#> [1] "matrix"
dense_matrix
#> DelayedMatrix object of 20000 x 600 doubles:
#>                [,1]       [,2]       [,3] ...     [,599]     [,600]
#>     [1,] 0.92832478 0.03727067 0.42298066   . 0.26919993 0.01088585
#>     [2,] 0.66435154 0.32407324 0.52703955   . 0.61336481 0.02026626
#>     [3,] 0.11287813 0.81287740 0.66877786   . 0.26286243 0.24718550
#>     [4,] 0.43659791 0.18193535 0.48888907   . 0.50910651 0.30005438
#>     [5,] 0.02471516 0.76051861 0.47671737   . 0.95958493 0.12507951
#>      ...          .          .          .   .          .          .
#> [19996,] 0.79903182 0.77150048 0.47481631   .  0.2592853  0.5643751
#> [19997,] 0.04831145 0.18305638 0.31878865   .  0.4941530  0.1577152
#> [19998,] 0.84171147 0.52087414 0.11779283   .  0.6153694  0.7371335
#> [19999,] 0.35407359 0.90328605 0.83162674   .  0.9211680  0.7909079
#> [20000,] 0.06169634 0.39515889 0.34669773   .  0.2534062  0.6329015
microbenchmark(DelayedArray::colSums(dense_matrix),
               DelayedMatrixStats::colSums2(dense_matrix),
               times = 10)
#> Unit: milliseconds
#>                                        expr        min         lq
#>         DelayedArray::colSums(dense_matrix) 4623.47451 5107.38358
#>  DelayedMatrixStats::colSums2(dense_matrix)   13.45209   14.76281
#>        mean     median         uq        max neval
#>  6338.87961 6273.05437 6934.66140 9299.03339    10
#>    23.64239   20.84275   32.58026   43.01038    10
profmem::total(profmem::profmem(DelayedArray::colSums(dense_matrix)))
#> [1] 2498163168
profmem::total(profmem::profmem(DelayedMatrixStats::colSums2(dense_matrix)))
#> [1] 165512

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
#>                [,1]       [,2]       [,3] ...    [,599]    [,600]
#>     [1,] 0.92832478 0.03727067 0.42298066   . 0.2691999 0.0000000
#>     [2,] 0.66435154 0.32407324 0.52703955   . 0.0000000 0.0000000
#>     [3,] 0.00000000 0.00000000 0.00000000   . 0.2628624 0.0000000
#>     [4,] 0.00000000 0.00000000 0.00000000   . 0.5091065 0.0000000
#>     [5,] 0.02471516 0.76051861 0.00000000   . 0.9595849 0.0000000
#>      ...          .          .          .   .         .         .
#> [19996,] 0.00000000 0.00000000 0.00000000   . 0.2592853 0.0000000
#> [19997,] 0.04831145 0.00000000 0.31878865   . 0.0000000 0.0000000
#> [19998,] 0.84171147 0.00000000 0.00000000   . 0.6153694 0.0000000
#> [19999,] 0.00000000 0.90328605 0.00000000   . 0.0000000 0.0000000
#> [20000,] 0.00000000 0.00000000 0.00000000   . 0.2534062 0.0000000
microbenchmark(DelayedArray::colSums(sparse_matrix),
               DelayedMatrixStats::colSums2(sparse_matrix),
               times = 10)
#> Unit: milliseconds
#>                                         expr        min        lq
#>         DelayedArray::colSums(sparse_matrix) 1128.75228 1141.4802
#>  DelayedMatrixStats::colSums2(sparse_matrix)   11.07331   11.4421
#>        mean     median        uq        max neval
#>  1322.53566 1267.81484 1311.1807 2026.94666    10
#>    13.48079   12.72859   14.6371   19.59188    10
profmem::total(profmem::profmem(DelayedArray::colSums(sparse_matrix)))
#> [1] 1709259576
profmem::total(profmem::profmem(DelayedMatrixStats::colSums2(sparse_matrix)))
#> [1] 5464

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
#>       [1,]    2    1    2    2    1    1
#>       [2,]    2    1    2    2    1    1
#>       [3,]    2    1    2    2    1    1
#>       [4,]    2    1    2    2    1    1
#>       [5,]    2    1    2    2    1    1
#>        ...    .    .    .    .    .    .
#> [1999996,]    1    1    2    1    1    1
#> [1999997,]    1    1    2    1    1    1
#> [1999998,]    1    1    2    1    1    1
#> [1999999,]    1    1    2    1    1    1
#> [2000000,]    1    1    2    1    1    1
microbenchmark(DelayedArray::colSums(rle_matrix),
               DelayedMatrixStats::colSums2(rle_matrix),
               times = 10)
#> Unit: milliseconds
#>                                      expr         min          lq
#>         DelayedArray::colSums(rle_matrix) 2440.773690 2479.701479
#>  DelayedMatrixStats::colSums2(rle_matrix)    3.800472    4.164246
#>        mean      median         uq        max neval
#>  2535.94410 2500.417820 2587.09763 2688.49354    10
#>    11.24623    4.421598    5.00898   51.57551    10
profmem::total(profmem::profmem(DelayedArray::colSums(rle_matrix)))
#> [1] 787432264
profmem::total(profmem::profmem(DelayedMatrixStats::colSums2(rle_matrix)))
#> [1] 1872
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
