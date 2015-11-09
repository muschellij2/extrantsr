#' @title Two-Stage Multi-Registration of images
#'
#' @description Takes a list of images and registers them to a template, 
#' takes a statistic image, and registers the list of images back to this 
#' statistic image.
#' @param infiles Input image files
#' @param template.file Template image to register to for first pass.
#' Second pass will be statistic image
#' @param func Function to be passed to 
#' @param ... Arguments to be passed to \code{\link{multi_reg}}. 
#' @note The second stage will use the same arguments passed in \code{...}, but
#' the \code{template.file} will be the statistic image
#' @export
#' @return Output list of statistic image, 
#' registered images to statistic image 
#' and transformations
multi_rereg <- function(infiles, 
                        template.file,
                        func = "mean",
                        ...){
  
  res = multi_reg(infiles = infiles, template.file = template.file, ...)
  outfiles = lapply(res, function(x) x$outfile)
  run_stat_img = stat_img(outfiles, func = func)
  res = multi_reg(infiles = infiles, template.file = run_stat_img, ...)
  
  L = list(stat_img = run_stat_img, results = res)
  return(L)
}

