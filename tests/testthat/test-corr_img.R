test_that("corr_img works", {
  arr = lapply(1:10, function(x) {
    oro.nifti::nifti(array(as.numeric(rnorm(20^3) > 0.25), dim = rep(20, 3)))
  })
  out = corr_img(arr[[1]], arr[[2]])
  testthat::expect_s4_class(out, "nifti")
})
