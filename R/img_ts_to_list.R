#' @title Image Time Series to list
#' @description Turns a 4D time series image to a list of 3D images
#' @param imgs object of class \code{\link{nifti}} with 4 dimensions, 
#' aka a 4D time series
#' @param copy_nifti Should \code{nifti} objects be returned (\code{TRUE}) or 
#' simply arrays (\code{FALSE}).  Should only be used for slight speed up when
#' array is adequate
#' @param warn Should a warning be printed if object is not class
#' \code{\link{nifti}}
#'
#' @return List of images
#' @note If the object is not of class \code{\link{nifti}} or have
#' 4 dimensions, then the object is returned
#' @export
img_ts_to_list = function(imgs, copy_nifti = TRUE, warn = TRUE) {
  
  if (length(dim(imgs)) == 4) {
    if (is.nifti(imgs)) {
      L = apply(imgs, 4, list)
      if ( copy_nifti ) {
        L = lapply(L, function(x) {
          copyNIfTIHeader(imgs, x[[1]])
        })
      } else {
        L = lapply(L, function(x) {
          x[[1]]
        })
      }
    } else {
      if (warn) {
        warning("Object is not of class nifti")
      }
      return(imgs)
    }
  } else {
    return(imgs)
  }
}


#' @title Image List to Time Series
#' @description Turns a a list of 3D images into a 4D time series image 
#' @param imgs object of class \code{\link{list}}, each with 3 dimensions, 
#' @param copy_nifti Should a \code{nifti} object be returned (\code{TRUE}) or 
#' a simply array (\code{FALSE}).  Should only be used for slight speed up when
#' array is adequate
#' @param warn Should a warning be printed if object is not class
#' \code{\link{nifti}} 
#' @importFrom abind abind
#' @return Object of class \code{\link{nifti}}
#' @note If the object is not of class \code{\link{list}},
#' then the object is returned 
#' @export
img_list_to_ts = function(imgs, copy_nifti = TRUE, warn = TRUE) {
  
  if (!is.list(imgs)) {
    if (warn){
      warning("Not a list, cowardly returning object")
    }
    return(imgs)
  }
  dims = lapply(imgs, dim)
  d1 = dims[[1]]
  check = sapply(dims, function(x){
    if (length(x) != length(d1)) { 
      return(FALSE)
    }
    all(x == d1)
  })
  if (!all(check)) {
    stop("Not all dimensions are equal")
  }
  
  arr = do.call("abind", list(... = imgs, along = 4))
  img1 = imgs[[1]]
  
  if ( copy_nifti ) {
    if (is.nifti(img1)) {
      arr = copyNIfTIHeader(img1, arr)
    } else {
      if (warn) {
        warning("Object is not of class nifti")
      }
    }
  }
  return(arr)
}



#' @title Image Time Series to Matrix
#' @description Turns a 4D time series image to a Matrix
#' @param imgs object of class \code{\link{nifti}} with 4 dimensions, 
#' aka a 4D time series
#' @param warn Should a warning be printed if object is not class
#' \code{\link{nifti}} (e.g. a list instead)
#' @return Matrix of values
#' @export
img_ts_to_matrix = function(imgs, warn = FALSE) {
  
  img_list = img_ts_to_df(imgs)
  img_list = as.matrix(img_list)

}

#' @title Image Time Series to Data.frame
#' @description Turns a 4D time series image to a Data.frame
#' @param imgs object of class \code{\link{nifti}} with 4 dimensions, 
#' aka a 4D time series
#' @param warn Should a warning be printed if object is not class
#' \code{\link{nifti}} (e.g. a list instead)
#' @return Matrix of values
#' @export
img_ts_to_df = function(imgs, warn = FALSE) {
  
  img_list = img_ts_to_list(imgs, copy_nifti = FALSE, warn = warn)
  list_names = names(img_list)
  img_list = lapply(img_list, c)
  img_list = as.data.frame(img_list)
  colnames(img_list) = list_names
  return(img_list)
}