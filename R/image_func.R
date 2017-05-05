#' @title Create Statistics Image
#' @description Creates output from a list of images, performing voxel-wise
#' operations to create a statistic image
#' @param imgs Character vector, list of characters, or object of class
#' \code{nifti}
#' @param func Function to perform voxel-wise on list of images
#' @param finite Should non-finite values be removed?
#' @param masks Character vector, list of characters, or object of class
#' \code{nifti} same length as images.  
#' @param na_masks if \code{masks} are given, should values of \code{0} be
#' turned into \code{NA}?
#' @param ... Addictional arguments to pass to \code{func}
#' @return Object of class \code{nifti}
#' @export
#' @note When \code{func = "mode"}, the data is tabulated and then
#' \code{\link{max.col}} is run.  The user can pass \code{ties.method} to
#' determine how to break ties.  The default is \code{"first"} as compared to
#' \code{\link{max.col}} where it is \code{"random"}.  When \code{func = "peak"},
#' \code{\link{density}} is run on each voxel and the value with the maximum.  If
#' the number of unique values is only 1, that value is returned.
#' @importFrom stats density
#' @importFrom matrixStats rowMedians rowSds 
#' @importFrom matrixStats rowVars rowMads rowProds rowQuantiles rowTabulates
#' @importFrom neurobase datatyper
stat_img = function(imgs,
                    func = c("mean",
                             "median",
                             "mode",
                             "pct",
                             "peak",
                             "sd",
                             "var",
                             "mad",
                             "sum",
                             "prod",
                             "z",
                             "quantile"),
                    finite = TRUE,
                    masks = NULL,
                    na_masks = TRUE,
                    ...)
{
  if (!is.character(func)) {
    stop("func must be of type character")
  }
  
  rowZs = function(x){
    rowMeans(x)/rowSds(x)
  }
  
  rowPeaks = function(x, ...) {
    apply(x, 1, function(r) {
      ur = unique(r)
      if (length(ur) == 1) {
        res = ur[1]
      } else {
        d = density(r, bw = "SJ", ...)
        res = d$x[which.max(d$y)]
      }
      return(res)
    })
  }
  
  rowModes = function(x, ties.method = "first", run_tab = TRUE ){
    
    if (run_tab) {
      is.wholenumber <- function(x, tol = .Machine$double.eps ^ 0.5){
        abs(x - round(x)) < tol
      }
      stopifnot(all(is.wholenumber(x)))
      
      x = array(as.integer(x), dim = dim(x))
      tabs = rowTabulates(x)
      cn = as.integer(colnames(tabs))
      ind = max.col(tabs, ties.method = ties.method)
      labs = cn[ind]
    } else {
      labs = max.col(x, ties.method = ties.method)
    }
    return(labs)
  }
  
  rowPcts = function(x){
    
    is.wholenumber <- function(x, tol = .Machine$double.eps ^ 0.5){
      abs(x - round(x)) < tol
    }
    stopifnot(all(is.wholenumber(x)))
    
    x = array(as.integer(x), dim = dim(x))
    tabs = rowTabulates(x)
    n = rowSums(tabs)
    labs = tabs / n
    
    return(labs)
  }
  
  rowNuniques = function(x) {
    apply(x, 1, function(r) length(unique(r)))
  }
  
  
  func = match.arg(func, several.ok = TRUE)
  
  ##########################################
  # Make a large matrix of images
  ##########################################
  imgs = check_nifti(imgs, allow.array = TRUE)
  imgs = img_ts_to_list(imgs, copy_nifti = TRUE, warn = FALSE)
  nim = imgs[[1]]
  stopifnot(length(imgs) > 1)
  dims = lapply(imgs, dim)
  same_dim = sapply(dims, all.equal, dims[[1]])
  if (!all(same_dim)) {
    stop("Not all image dimensions are identical!")
  }
  if (!is.null(masks)) {
    masks = check_nifti(masks, allow.array = TRUE )
    masks = img_ts_to_list(masks, copy_nifti = TRUE, warn = FALSE)  
    stopifnot(length(imgs) == length(masks))
    lapply(masks, check_mask_fail, 
           allow.NA = TRUE, 
           allow.array = TRUE)
    if (na_masks) {
      masks = lapply(masks, 
                     function(x){
                       x[ x == 0 ] = NA
                       return(x)
                     })
    }
    imgs = mapply(function(img, mask){
      mask_img(img, mask)
    }, imgs, masks, SIMPLIFY = FALSE)
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
    char_func = all.func[ifunc]
    func = switch(char_func,
                  mean = rowMeans,
                  median = rowMedians,
                  sd = rowSds,
                  var = rowVars,
                  mad = rowMads,
                  sum = rowSums,
                  prod = rowProds,
                  z = rowZs,
                  pct = rowPcts,
                  mode = rowModes,
                  peak = rowPeaks,
                  quantile = rowQuantiles)
    res_img = func(mat, ...)
    ##########################
    # pct and other functions return a 4d Image.
    ##########################
    if (!char_func %in% c("pct")) {
      if (length(res_img) != nrow(mat)) {
        stop(paste0("Function used did not result in a vector-",
                    "may need to pass more arguments, ",
                    "such as quantile needs to pass ONE prob"))
      }
      res_img = niftiarr(nim, res_img)
      if (finite) {
        res_img = neurobase::finite_img(res_img, replace = 0)
      }
      res_img = datatyper(res_img)
      
    } else {
      num_imgs = ncol(res_img)
      res_list = vector(mode = "list", length = num_imgs) 
      names(res_list) = colnames(res_list)
      for (icol in seq(num_imgs)) {
        x = res_img[, icol]
        x = niftiarr(nim, x)
        if (finite) {
          x = neurobase::finite_img(x, replace = 0)
        }
        x = datatyper(x)
        res_list[[icol]] = x
        rm(list = "x");
      }
      res_img = res_list
      rm(list = "res_list");
    }
    L[[ifunc]] = res_img
  }
  # return image if only one
  if (nfunc == 1) {
    L = L[[ifunc]]
  }
  return(L)
}