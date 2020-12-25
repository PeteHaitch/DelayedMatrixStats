# Some special tests for the row/colQuantiles functions.
# library(DelayedMatrixStats); library(testthat); source("test_quantile.R")

test_that("colQuantiles correctly handle the drop= argument", {
    x <- matrix(runif(100000), nrow=10)
    colnames(x) <- sprintf("THING%i", seq_len(ncol(x)))

    # Handles a length-1 prob.
    expect_identical(
        colQuantiles(DelayedArray(x) + 1, prob=0.5),
        colQuantiles(x + 1, prob=0.5)
    )
    
    expect_identical(
        colQuantiles(DelayedArray(x) + 1, prob=0.5, drop=FALSE),
        colQuantiles(x + 1, prob=0.5, drop=FALSE)
    )

    # Handles multiple dimensions correctly.
    multiple <- c(0.2, 0.5, 0.7)
    expect_identical(
        colQuantiles(DelayedArray(x) + 1, prob=multiple),
        colQuantiles(x + 1, prob=multiple)
    )
    
    expect_identical(
        colQuantiles(DelayedArray(x) + 1, prob=multiple, drop=FALSE),
        colQuantiles(x + 1, prob=multiple, drop=FALSE)
    )

    # Handling a 1-dimensional input.
    expect_identical(
        colQuantiles(DelayedArray(x)[,1,drop=FALSE], prob=multiple),
        colQuantiles(x[,1,drop=FALSE], prob=multiple)
    )
    
    expect_identical(
        colQuantiles(DelayedArray(x)[,1,drop=FALSE], prob=multiple, drop=FALSE),
        colQuantiles(x[,1,drop=FALSE], prob=multiple, drop=FALSE)
    )
})

test_that("rowQuantiles correctly handle the drop= argument", {
    x <- matrix(runif(100000), nrow=10)
    rownames(x) <- sprintf("THING%i", seq_len(nrow(x)))

    # Handles a length-1 prob.
    expect_identical(
        rowQuantiles(DelayedArray(x) + 1, prob=0.5),
        rowQuantiles(x + 1, prob=0.5)
    )
    
    expect_identical(
        rowQuantiles(DelayedArray(x) + 1, prob=0.5, drop=FALSE),
        rowQuantiles(x + 1, prob=0.5, drop=FALSE)
    )

    # Handles multiple dimensions properly.
    multiple <- c(0.2, 0.5, 0.7)
    expect_identical(
        rowQuantiles(DelayedArray(x) + 1, prob=multiple),
        rowQuantiles(x + 1, prob=multiple)
    )
    
    expect_identical(
        rowQuantiles(DelayedArray(x) + 1, prob=multiple, drop=FALSE),
        rowQuantiles(x + 1, prob=multiple, drop=FALSE)
    )

    # Handling a 1-dimensional input.
    expect_identical(
        rowQuantiles(DelayedArray(x)[1,,drop=FALSE], prob=multiple),
        rowQuantiles(x[1,,drop=FALSE], prob=multiple)
    )
    
    expect_identical(
        rowQuantiles(DelayedArray(x)[1,,drop=FALSE], prob=multiple, drop=FALSE),
        rowQuantiles(x[1,,drop=FALSE], prob=multiple, drop=FALSE)
    )
})
