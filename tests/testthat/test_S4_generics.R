context("S4 generics")

test_that("Formals of S4 generic matches matrixStats function", {
  # TODO: Better way to only get exported generics
  dms_generics <- getGenerics("package:DelayedMatrixStats")
  # TODO: Unsure why, but some (implicit?) generics from base are being picked
  #       up, namely `[`, `[[<-`, `[<-`, `$`, and `$<-`
  dms_generics <- dms_generics[dms_generics@package != "base"]
  dms_generics <- setdiff(dms_generics, "subset_by_Nindex")
  lapply(dms_generics, function(dms_generic) {
    ms_fun <- utils::getFromNamespace(dms_generic, "matrixStats")
    expect_identical(formals(dms_generic), formals(ms_fun))
  })
})
