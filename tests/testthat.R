library(testthat)
library(DelayedMatrixStats)

test_check("DelayedMatrixStats")
setDefaultBlockSize(8)
test_check("DelayedMatrixStats")
