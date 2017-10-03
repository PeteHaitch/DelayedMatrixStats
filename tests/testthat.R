library(testthat)
library(DelayedMatrixStats)

test_check("DelayedMatrixStats")
options(DelayedArray.block.size = 8L)
test_check("DelayedMatrixStats")
