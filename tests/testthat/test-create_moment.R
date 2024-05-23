test_that("create_moment works", {
  arr = oro.nifti::nifti(array(rnorm(20^3), dim = rep(20, 3)))
  res = create_moment(arr)
  testthat::expect_s4_class(res$mn, "nifti")
  testthat::expect_named(res, c("mn", "sd", "skew", "kurt", "grad", "z"))
})
