#' @title Image Mask Filler
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
filler <- function(img, 
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
                            arr = arr)
    }
    if (is.antsImage(img)) {
      # coarse_ants = function(img, reference) {
      #   arr = as(img, Class = "array")
      #   aimg = as.antsImage(arr)
      #   antsCopyImageInfo(
      #     target = aimg, 
      #     reference = reference)
      #   return(aimg)
      # }          
      # arr = coarse_ants(arr, 
      #                   reference = img)      
      arr = oro2ants(arr, reference = img)
    }
    # is_niftiImage = function(x) {
    #   inherits(x, "niftiImage")
    # }    
    # if (is_niftiImage(img)) {
    #   arr = updateNifti(arr, template = img)
    # }
    return(arr)
  }
  
  # ensure_array = function(x) {
  #   x = x %>% 
  #     as.array %>% 
  #     as(Class = "array")
  #   return(x)
  # }
  
  if (is.character(img)) {
    img = check_nifti(img)
  }
  filled = img %>% 
    ensure_array %>% 
    zero_pad(kdim = kdim) %>% 
    cnif(img = img,  verbose = verbose) 
  if (dilate) {
    filled = filled %>% 
      oMath("MD", fill_size, 
            retfile = TRUE) 
  }
  if (erode) {
    filled = filled %>% 
      oMath("ME", fill_size, 
            retfile = TRUE) 
  } 
  filled = filled %>% 
    ensure_array %>%          
    zero_pad(
      kdim = kdim, 
      invert = TRUE) %>% 
    cnif(img = img, verbose = verbose)
  
  return(filled)
}