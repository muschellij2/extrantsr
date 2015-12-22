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
#' @note When \code{func = "mode"}, the data is tabulated and then
#' \code{\link{max.col}} is run.  The user can pass \code{ties.method} to 
#' determine how to break ties.  The default is \code{"first"} as compared to 
#' \code{\link{max.col}} where it is \code{"random"}.
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
  
  rowZs = function(x){
    rowMeans(x)/rowSds(x)
  }
  
  rowModes = function(x, ties.method = "first" ){
    is.wholenumber <- function(x, tol = .Machine$double.eps ^ 0.5){
      abs(x - round(x)) < tol
    }
    stopifnot(all(is.wholenumber(x)))
    
    x = array(as.integer(x), dim = dim(x))
    tabs = rowTabulates(x)
    cn = as.integer(colnames(tabs))
    ind = max.col(tabs, ties.method = ties.method)
    labs = cn[ind]
    labs
  }  
  
  
  func = match.arg(func, several.ok = TRUE)
  
  ##########################################
  # Make a large matrix of images
  ##########################################  
  imgs = check_nifti(imgs)
  imgs = img_ts_to_list(imgs, warn = FALSE)
  nim = imgs[[1]]
  dims = lapply(imgs, dim)
  same_dim = sapply(dims, all.equal, dims[[1]])
  if (!all(same_dim)) {
    stop("Not all image dimensions are identical!")
  }
  mat = lapply(imgs, c)
  mat = do.call("cbind", mat)
  stopifnot(nrow(mat) == prod(dims[[1]]))
  
  ##########################################
  # Run through all functions
  ##########################################    
  all.func = func
  nfunc = length(all.func)
  L = vector(mode = "list", 
             length = nfunc)
  names(L) = all.func
  for (ifunc in seq(nfunc)) {
    func = switch(all.func[ifunc],
                  mean = rowMeans,
                  median = rowMedians,
                  sd = rowSds,
                  var = rowVars,
                  mad = rowMads,
                  sum = rowSums,
                  prod = rowProds,
                  z = rowZs,
                  mode = rowModes)  
    res_img = func(mat, ...)
    res_img = niftiarr(nim, res_img)
    if (finite) {
      res_img = fslr::finite_img(res_img, replace = 0)
    }
    L[[ifunc]] = res_img
  }
  # return image if only one
  if (nfunc == 1) {
    L = L[[ifunc]]
  }
  return(L)
}