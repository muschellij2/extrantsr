#' @title Image Mask Filler
#' @description Fills in a binary image using \code{\link{iMath}}, 
#' using dilation erosion but with zero padding built in.
#' @param img Object of class \code{\link{nifti}} or \code{antsImage}
#' @param fill_size Size of fill (in voxels)
#' @param verbose Output diagnostic values
#' @return Filled object with same class as \code{img}
#' @export
#' @import ANTsR
filler <- function(img, 
                  fill_size = 7,
                  verbose = TRUE){
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
    return(arr)
  }
  
  filled = img %>% 
    as.array %>% 
    as(Class = "array") %>% 
    zero_pad(kdim = kdim) %>% 
    cnif(img = img,  verbose = verbose) %>% 
    oMath("MD", fill_size, 
          retfile = TRUE) %>% 
    oMath("ME", fill_size, 
          retfile = TRUE) %>% 
    as.array %>% 
    as(Class = "array") %>%             
    zero_pad(
      kdim = kdim, 
      invert = TRUE) %>% 
    cnif(img = img, verbose = verbose)
  
  return(filled)
}