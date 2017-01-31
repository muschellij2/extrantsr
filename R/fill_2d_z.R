#' @title Image Mask Filler over each slice in Z-direction
#' @description Fills in a binary image using \code{\link{iMath}}, 
#' using dilation erosion but with zero padding built in.
#' @param img Object of class \code{\link{nifti}} or \code{antsImage}
#' @param fill_size Size of fill (in voxels)
#' @param verbose Output diagnostic values
#' @param dilate should the image be dilated.  This and \code{erode} 
#' allows for the function to perform erosion/dilation only with zero-padding.
#' @param erode should the image be eroded  This and \code{dilate} 
#' allows for the function to perform erosion/dilation only with zero-padding.
#' @return Filled object with same class as \code{img}
#' @export
fill_2d_z <- function(
  img, 
  fill_size = 7,
  verbose = TRUE,
  dilate = TRUE,
  erode = TRUE){
  # kdim Dimensions to be padded to the image (in voxels)
  kdim = rep(fill_size, 3) * 2 + 1
  
  cnif = function(arr, img, verbose = TRUE) {
    if (verbose) {
      message("copying header")
    }
    if (is.nifti(img)) {
      arr = copyNIfTIHeader(img = img, 
                            arr = arr,
                            drop = FALSE)
    }
    if (is.antsImage(img)) {
      arr = oro2ants(arr, reference = img)
    }
    return(arr)
  }
  
  ensure_array = function(x) {
    x = x %>% 
      as.array %>% 
      as(Class = "array")
    return(x)
  }
  
  filled = img %>% 
    ensure_array %>% 
    zero_pad(kdim = kdim)
  dimg = dim(filled)
  d3 = dimg[3]
  
  idim = 1
  if (verbose) {
    pb = txtProgressBar(min = 0, max = d3, style = 3)
  }
  for (idim in seq(d3)) {
    arr = filled[,,idim, drop = FALSE]
    arr = cnif(arr, img = img, verbose = FALSE)
    if (dilate) {
      arr = arr %>% 
        oMath("MD", fill_size, 
              retfile = TRUE) 
    }
    if (erode) {
      arr = arr %>% 
        oMath("ME", fill_size, 
              retfile = TRUE) 
    }
    filled[,,idim] = arr[,,1]
    if (verbose) {
      setTxtProgressBar(pb, value = idim)
    }
  }
  if (verbose) {
    close(pb)
  }

  filled = filled %>% 
    ensure_array %>%          
    zero_pad(
      kdim = kdim, 
      invert = TRUE) %>% 
    cnif(img = img, verbose = verbose)
  
  return(filled)
}