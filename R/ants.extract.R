
#' @title Extract values from antsImage from mask
#'
#' @description Just helpful wrapper to convert to array and then take
#' values for which mask is 1
#' @param img image either in array or class \code{antsImage}
#' @param mask binary image either in array or class \code{antsImage}
#' @return Numeric vector
#' @export
ants.extract <- function(
  img, # image either in array or class \code{antsImage}
  mask # binary image either in array or class \code{antsImage}
){
  img = as.array(img)
  mask = as.array(mask)
  img[ mask == 1]
}

