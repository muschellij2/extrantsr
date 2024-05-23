test_that("otropos works", {
  arr = oro.nifti::nifti(array(rnorm(20^3), dim = rep(20, 3)))
  res = otropos2(arr)
  testthat::expect_type(res, "list")
  testthat::expect_named(res, c("segmentation", "probabilityimages"))
  
  testthat::expect_s4_class(res$segmentation, "nifti")
  testthat::expect_type(res$probabilityimages, "list")
  testthat::expect_equal(length(res$probabilityimages), 3L)
})
