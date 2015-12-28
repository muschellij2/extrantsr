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
  .Deprecated("antsApplyTransforms", package = "ANTsR")
  #   moving = tempants(moving)
  #   fixed = tempants(fixed)
  #   output = tempants(output)
  #   
  #   args <- list(dimension, fixed, output,
  #                R = moving, 
  #                i = transformlist)
  #   myargs <- sys_int_antsProcessArguments(c(args))
  #   myargs = myargs[ myargs != '-']
  # 
  #   myargs = paste(myargs, collapse=" ")
  #   
  #   cmd = paste0("WarpImageMultiTransform ", 
  #                myargs)
  #   system(cmd)
  
}


#' @title N4BiasCorrect with Bias Field
#'
#' @description N4BiasCorrect_WithField
#' @param img	input antsImage
#' @param mask input mask, if one is not passed one will be made
#' @param shrinkFactor Shrink factor for multi-resolution correction, 
#' typically integer less than 4
#' @param convergence List of: iters, vector of maximum number of iterations 
#' for each shrinkage factor, and tol, the convergence tolerance.
#' @param splineParam Parameter controlling number of control points in spline. 
#' Either single value, indicating how many control points, or vector with one 
#' entry per dimension of image, indicating the spacing in each direction.
#' @param ... options to be passed to \code{n4BiasFieldCorrection}, such as
#' \code{v = 1} for verbose.
#' @export
#' @return List of output image, field image, and mask.
N4BiasCorrect_WithField <- function(img, mask = NA, shrinkFactor = 4, 
                                     convergence = list(
                                       iters = c(50, 50, 50, 50), 
                                       tol = 1e-07), 
                                     splineParam = 200, ...) {
  if (!is.antsImage(mask)) 
    mask <- getMask(img)
  N4_CONVERGENCE_1 <- paste("[", paste(convergence$iters, collapse = "x"), 
                            ",", sprintf("%.10f", convergence$tol), "]", sep = "")
  N4_SHRINK_FACTOR_1 <- paste(shrinkFactor)
  if (length(splineParam) == 1) {
    N4_BSPLINE_PARAMS <- paste("[", splineParam, "]", sep = "")
  }
  else if (length(splineParam) == img@dimension) {
    N4_BSPLINE_PARAMS <- paste("[", paste(splineParam, collapse = "x"), 
                               "]", sep = "")
  }
  else {
    stop("Length of splineParam must either be 1 or dimensionality of image.")
  }
  
  tfile = tempants(img)
  field_fname = tempfile(fileext = ".nii.gz")
  o = paste0("[", tfile, ",",
             field_fname,
             "]")  
  
  ANTsR:::.helpn4BiasFieldCorrection(list(d = img@dimension, i = img, 
                                          s = N4_SHRINK_FACTOR_1, c = N4_CONVERGENCE_1, b = N4_BSPLINE_PARAMS, 
                                          x = mask, o = o))
  
  outimg = antsImageRead(tfile)
  field = antsImageRead(field_fname)
  res = list(outimg = outimg, field = field, mask = mask)
  
  #   
  #   outimg <- antsImageClone(img)
  #   ANTsR:::.helpn4BiasFieldCorrection(
  #     list(d = outimg@dimension, 
  #          i = img, 
  #          s = N4_SHRINK_FACTOR_1, 
  #          c = N4_CONVERGENCE_1, 
  #          b = N4_BSPLINE_PARAMS, 
  #          x = mask, o = o, v = 1))  
  
  # .Deprecated("n4BiasFieldCorrection", package = "ANTsR")
  #   
  #   myargs <- sys_int_antsProcessArguments(c(args))
  #   myargs = myargs[ myargs != '-']
  #   myargs = paste(myargs, collapse="")
  #   cmd = paste0("N4BiasFieldCorrection ", 
  #                myargs)
  #   system(cmd)
  return(res)
}


#' @title Parse ANTs Arguments for system
#'
#' @description Options for using ANTs and system
#' @param args Arguments that must be labeled and be passed to 
#' N4BiasFieldCorrection
#' @export
#' @return Character vector
sys_int_antsProcessArguments = function (args) 
{
  .Deprecated(".int_antsExtractXptrAsString", "ANTsR",
              msg = "Not used, ANTsR Update made this obsolete")
  #   if (typeof(args) == "list") {
  #     char_vect <- NULL
  #     for (i in (1:length(args))) {
  #       if (length(names(args)) != 0) {
  #         if (nchar(names(args)[i]) > 1) {
  #           char_vect <- c(char_vect, paste("--", names(args)[i], 
  #                                           sep = ""))
  #         }
  #         else {
  #           char_vect <- c(char_vect, paste("-", names(args)[i], 
  #                                           sep = ""))
  #         }
  #       }
  #       if (typeof(args[[i]]) == "list") {
  #         char_vect <- c(char_vect, paste(args[[i]]$name, 
  #                                         "[", sep = ""))
  #         args[[i]]$name <- NULL
  #         adder = 
  #         for (j in (1:length(args[[i]]))) {
  #           adder = ","
  #           if (j == length(args[[i]])){
  #             adder = ""
  #           }
  #           char_vect <- c(char_vect, 
  #                          paste0(as.character(ANTsR:::.int_antsExtractXptrAsString(
  #                            args[[i]][[j]])), adder))
  #         }
  #         char_vect <- c(char_vect, "]")
  #       }
  #       else {
  #         char_vect <- c(char_vect, as.character(ANTsR:::.int_antsExtractXptrAsString(args[[i]])))
  #       }
  #     }
  #   }
  #   starter = which(char_vect == "[")
  #   stopper = which(char_vect == "]")
  #   stopifnot(length(starter) == length(stopper))
  #   inds = NULL
  #   for (i in seq_along(starter)){
  #     istart = starter[i]
  #     istop = stopper[i] - 1
  #     inds = c(inds, seq(istart, istop))
  #   }
  #   all.inds = seq_along(char_vect)
  #   spacer = !(all.inds %in% inds)
  #   char_vect[ spacer ] = paste0(char_vect[ spacer ], " ")
  #   return(char_vect)
}