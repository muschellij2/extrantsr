#' @title Inpainting of images 
#'
#' @description Paints in the image with the mean/median from 
#' \code{fill_mask} 
#' @param img image of class \code{nifti} or character
#' @param input_mask mask to be painted in.
#' @param fill_mask image mask to take the function (e.g. mean/median) of the image
#' @param func Function to apply on the values.  \code{do.call} will be called on them
#' @param ... Arguments to be passed to \code{func}. 
#' @export
#' @return List of output image (\code{outimg}) and 
#' statistic filled in (\code{statistic})
inpaint <- function(img, 
                    input_mask,
                    fill_mask,
                    func = c("mean", "median"),
                    ...){
  
  img = check_nifti(img)
  input_mask = check_nifti(input_mask) > 0
  fill_mask = check_nifti(fill_mask) > 0
  
  ind = which(fill_mask > 0)
  fill_vals = img[ ind ]
  stat = do.call(func, fill_vals, ...)
  
  outimg = img
  outimg[ input_mask ] = stat
  outimg = cal_img(outimg)
  
  L = list(outimg = outimg, statistic = stat)
  return(L)
}

