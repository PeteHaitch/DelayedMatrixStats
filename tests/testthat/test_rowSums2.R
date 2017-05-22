#-------------------------------------------------------------------------------
# Non-exported methods
#

test_that(".rowSums2() conforms to matrixStats API", {
  lapply(setdiff(seed_types, "matrix"), function(seed_type) {
    # NOTE: Only test those for which object exists (e.g. there is no 'empty'
    #       data.frame seed, so want to skip that)
    objects <- unlist(x = list_of_seeds[[seed_type]], recursive = FALSE)
    expecteds <- unlist(x = list_of_seeds[["matrix"]], recursive = FALSE)
    expecteds <- expecteds[match(names(objects), names(expecteds))]
    mapply(function(object, expected, i) {
      expect_identical(.rowSums2(object), .rowSums2(expected))
  }, object = objects, expected = expecteds, i = seq_along(objects))
  })
})
