test_that("subset_4d antsImage works", {
  arr = array(rnorm(20^4), dim = rep(20, 4))
  aimg = as.antsImage(arr)
  ind = 2:3
  res = subset_4d(aimg, ind)
  testthat::expect_true(all(dim(res) == c(20, 20, 20, 2)))
  res = subset_4d(aimg, ind = 1)
  testthat::expect_true(all(dim(res) == c(20, 20, 20)))
  
  
  arr = array(rnorm(20^3), dim = rep(20, 3))
  aimg = as.antsImage(arr)
  testthat::expect_error(subset_4d(aimg, ind = 1), regexp = "4D")
    
})
