
#' @title Inverse Warp from antsRegistration
#'
#' @description A simplified (or full) interface to 
#' WarpImageMultiTransform.
#' @param dimension Number of dimensions of the input image
#' @param fixed fixed image to which we register the moving image.
#' @param output output filename
#' @param moving moving image to be mapped to fixed space.
#' @param transformlist list of transforms usually from \code{antsRegistration}
#' @export
#' @return Result of \code{system} command
invwarp <- function(
  dimension, # Number of dimensions of the input image
  fixed, # fixed image to which we register the moving image.
  output, # output filename
  moving, # moving image to be mapped to fixed space.
  transformlist # list of transforms, usually from \code{antsRegistration}
){
  
  moving = tempants(moving)
  fixed = tempants(fixed)
  output = tempants(output)
  
  args <- list(dimension, fixed, output,
               R = moving, 
               i = transformlist)
  myargs <- int_antsProcessArguments(c(args))
  myargs = myargs[ myargs != '-']
  
  cmd = paste0("WarpImageMultiTransform ", 
               paste(myargs, collapse=" "))
  system(cmd)
  
}