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
  myargs <- sys_int_antsProcessArguments(c(args))
  myargs = myargs[ myargs != '-']

  myargs = paste(myargs, collapse=" ")
  
  cmd = paste0("WarpImageMultiTransform ", 
               myargs)
  system(cmd)
  
}


#' @title N4BiasCorrect with Bias Field
#'
#' @description N4BiasCorrect_WithField
#' @param args Arguments that must be labeled and be passed to 
#' N4BiasFieldCorrection
#' @export
#' @return Result of \code{system} command
N4BiasCorrect_WithField <- function(
 args
){

  myargs <- sys_int_antsProcessArguments(c(args))
  myargs = myargs[ myargs != '-']
  myargs = paste(myargs, collapse="")
  cmd = paste0("N4BiasFieldCorrection ", 
               myargs)
  system(cmd)
  
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
  if (typeof(args) == "list") {
    char_vect <- NULL
    for (i in (1:length(args))) {
      if (length(names(args)) != 0) {
        if (nchar(names(args)[i]) > 1) {
          char_vect <- c(char_vect, paste("--", names(args)[i], 
                                          sep = ""))
        }
        else {
          char_vect <- c(char_vect, paste("-", names(args)[i], 
                                          sep = ""))
        }
      }
      if (typeof(args[[i]]) == "list") {
        char_vect <- c(char_vect, paste(args[[i]]$name, 
                                        "[", sep = ""))
        args[[i]]$name <- NULL
        adder = 
        for (j in (1:length(args[[i]]))) {
          adder = ","
          if (j == length(args[[i]])){
            adder = ""
          }
          char_vect <- c(char_vect, 
                         paste0(as.character(int_antsExtractXptrAsString(
                           args[[i]][[j]])), adder))
        }
        char_vect <- c(char_vect, "]")
      }
      else {
        char_vect <- c(char_vect, as.character(int_antsExtractXptrAsString(args[[i]])))
      }
    }
  }
  starter = which(char_vect == "[")
  stopper = which(char_vect == "]")
  stopifnot(length(starter) == length(stopper))
  inds = NULL
  for (i in seq_along(starter)){
    istart = starter[i]
    istop = stopper[i] - 1
    inds = c(inds, seq(istart, istop))
  }
  all.inds = seq_along(char_vect)
  spacer = !(all.inds %in% inds)
  char_vect[ spacer ] = paste0(char_vect[ spacer ], " ")
  return(char_vect)
}