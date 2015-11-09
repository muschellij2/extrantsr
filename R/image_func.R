#' @title Generic Image Function
#' @description Creates output from a list of images, performing voxel-wise
#' operations
#' @param imgs Character vector, list of characters, or object of class 
#' \code{nifti}
#' @param func Function to perform voxel-wise on list of images 
#' @param finite Should non-finite values be removed?
#' @param ... Addictional arguments to pass to \code{func}
#' @import matrixStats
#' @return Object of class \code{nifti}
#' @export
image_func = function(imgs, 
                      func = c("mean", 
                               "median",
                               "sd",
                               "var",
                               "mad",
                               "sum",
                               "prod",
                               "z"),
                      finite = TRUE,
                      ...)
  {
  if (!is.character(func)) {
    stop("func must be of type character")
  }
  
  func = match.arg(func)
  rowZs = function(x){
    rowMeans(x)/rowSds(x)
  }
  func = switch(func,
                mean = rowMeans,
                median = rowMedians,
                sd = rowSds,
                var = rowVars,
                mad = rowMads,
                sum = rowSums,
                prod = rowProds,
                z = rowZs
  )
  
  imgs = check_nifti(imgs)
  nim = imgs[[1]]
  dims = lapply(imgs, dim)
  same_dim = sapply(dims, all.equal, dims[[1]])
  if (!all(same_dim)) {
    stop("Not all image dimensions are identical!")
  }
  mat = lapply(imgs, c)
  mat = do.call("cbind", mat)
  stopifnot(nrow(mat) == prod(dims[[1]]))
  res_img = func(mat, ...)
  res_img = niftiarr(nim, 
                     res_img)
  if (finite) {
    res_img = finite_img(res_img, replace = 0)
  }
  return(res_img)
}