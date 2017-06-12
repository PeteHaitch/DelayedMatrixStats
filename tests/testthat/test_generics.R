context("S4 generics")

test_that("Formals of S4 generic matches matrixStats function", {
  # TODO: Better way to only get exported generics
  dms_generics <- getGenerics("package:DelayedMatrixStats")
  dms_generics <- setdiff(dms_generics, "subset_simple_seed_as_seed_class")
  lapply(dms_generics, function(dms_generic) {
    ms_fun <- getFromNamespace(dms_generic, "matrixStats")
    expect_identical(formals(dms_generic), formals(ms_fun))
  })
})

# TODO: Test S4 methods have same args as generic?