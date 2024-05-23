testthat::test_that("ants2oro work", {
  arr = array(rnorm(20^3), dim = rep(20, 3))
  aimg = as.antsImage(arr)
  res = ants2oro(aimg)
  testthat::expect_s4_class(res, "nifti")
  
  
  testthat::expect_error(ants2oro(aimg, reference = aimg))
  testthat::expect_error(ants2oro(arr, reference = res))
  testthat::expect_error(ants2oro(arr))
  
  ants2oro(aimg, reference = res)
  
})
testthat::test_that("oro2ants works", {
  arr = array(rnorm(20^3), dim = rep(20, 3))
  nim = oro.nifti::nifti(arr)
  aimg = as.antsImage(arr)
  
  ### Or
  back = oro2ants(nim)
  testthat::expect_s4_class(back, "antsImage")
  
  oro2ants(aimg, reference = aimg)
  testthat::expect_error(oro2ants(arr, reference = nim))
  testthat::expect_error(oro2ants(arr))
  
})



testthat::test_that("finite_img/write_nifti works", {
  arr = array(rnorm(20^3), dim = rep(20, 3))
  arr[1, 3, 5] = NaN
  arr[1, 3, 7] = Inf
  arr[1, 3, 9] = -Inf
  arr[1, 3, 10] = NA
  aimg = as.antsImage(arr)
  
  testthat::expect_true(is.nan(as.array(aimg)[1,3,5]))
  testthat::expect_true(is.infinite(as.array(aimg)[1,3,7]))
  testthat::expect_true(is.infinite(as.array(aimg)[1,3,9]))
  # IT RETURNS NaN!!
  testthat::expect_true(is.na(as.array(aimg)[1,3,10]))
  testthat::expect_true(is.nan(as.array(aimg)[1,3,10]))
  
  out = finite_img(aimg)
  testthat::expect_true(!anyNA(as.array(out)))
  
  testthat::expect_true(out[1, 3, 5] ==0)
  testthat::expect_true(out[1, 3, 7] == 0)
  testthat::expect_true(out[1, 3, 9] == 0)
  testthat::expect_true(out[1, 3, 10] == 0)
  
  tfile = tempfile(fileext = ".nii")
  neurobase::write_nifti(aimg, tfile)
  testthat::expect_true(file.exists(tfile))
  testthat::expect_true(file.size(tfile) > 10)
})
