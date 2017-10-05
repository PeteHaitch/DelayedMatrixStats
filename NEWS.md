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
