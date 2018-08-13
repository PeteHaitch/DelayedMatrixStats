# DelayedMatrixStats 1.3.6

* Add `rowsum()` (based on `base::rowsum()`) and `colsum()` (not found in base R or **matrixStats**)

# DelayedMatrixStats 1.1.9

* Sync API with **matrixStats** v0.53.1
  * Add explicit arguments to col-/rowSds()
  * Fix dimnames for col-/rowQuantiles()
* Sync documentation with **matrixStats** v0.53.1

# DelayedMatrixStats 1.1.7

* Add `colWeightedSds()`, `colWeightedVars()`, `rowWeightedSds()`,  and `rowWeightedVars()`

# DelayedMatrixStats 1.1.6

* Add `colAvgsPerRowSet()`

# DelayedMatrixStats 1.1.5

* Defunct argument 'centers' for `colMads()`/`rowMads()` as of **matrixStats** v0.53.0
* Sync documentation with **matrixStats** v0.53.0

# DelayedMatrixStats 0.99.2

* Fix typos in vignette
* Allow vignette code chunks to error if they use functions from **profmem** (these only work if R was configured with support for memory-profiling, which isn't enabled on Bioconductor's malbec1 Linux machine)

# DelayedMatrixStats 0.99.2

* Update API table in README and vignette

# DelayedMatrixStats 0.99.1

* Develop against the current CRAN release of matrixStats instead of devel version
* Remove `colAvgsPerRowSet()` until next CRAN release of **matrixStats** (> v0.52.2; see [https://github.com/HenrikBengtsson/matrixStats/issues/110](https://github.com/HenrikBengtsson/matrixStats/issues/110)

# DelayedMatrixStats 0.99.0

* Initial submission to Bioconductor
