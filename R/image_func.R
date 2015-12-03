#' @title Create Statistics Image
#' @description Creates output from a list of images, performing voxel-wise
#' operations to create a statistic image
#' @param imgs Character vector, list of characters, or object of class 
#' \code{nifti}
#' @param func Function to perform voxel-wise on list of images 
#' @param finite Should non-finite values be removed?
#' @param ... Addictional arguments to pass to \code{func}
#' @import matrixStats
#' @return Object of class \code{nifti}
#' @export
stat_img = function(imgs, 
                    func = c("mean", 
                             "median",
                             "mode",
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
  
  rowModes = function(x){
    is.wholenumber <- function(x, tol = .Machine$double.eps ^ 0.5){
        abs(x - round(x)) < tol
    }
    stopifnot(all(is.wholenumber(x)))
    
    x = array(as.integer(x), 
              dim = dim(x))
    tabs = rowTabulates(x)
    cn = as.integer(colnames(tabs))
    ind = apply(tabs, 1, which.max)
    labs = cn[ind]
    labs
  }  
  func = switch(func,
                mean = rowMeans,
                median = rowMedians,
                sd = rowSds,
                var = rowVars,
                mad = rowMads,
                sum = rowSums,
                prod = rowProds,
                z = rowZs,
                mode = rowModes
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
    res_img = fslr::finite_img(res_img, replace = 0)
  }
  return(res_img)
}