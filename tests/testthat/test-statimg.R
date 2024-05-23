test_that("stat_img works", {
  
  arr = lapply(1:20, function(x) {
    oro.nifti::nifti(array(as.numeric(rnorm(20^3) > 0.25), dim = rep(20, 3)))
  })
  

  
  funcs = c("mean", "median", "mode", "sd", "var", "mad", 
            "sum", "prod", "z", "pct", "staple_prob", "staple_label")
  result = stat_img(arr,
           func = funcs)
  testthat::expect_named(result, funcs)
  
  
  testthat::expect_error(stat_img(arr, func = "quantile"))
  stat_img(arr, func = "quantile", probs = 0.99)
  
  
})
