#' @title Check if antsImage or read in
#' @description Simple check to see if input is character or of 
#' class antsImage
#' @return antsImage object
#' @seealso \link{antsImageRead}
#' @param x character path of image or 
#' an object of class antsImage
#' @param dimension (numeric) passed to 
#' \code{\link{antsImageRead}} if the image
#' is read in
#' @export
check_ants = function(x, dimension = 3){
  if (inherits(x, "character")) {
    img = antsImageRead(x, dimension = dimension)
  } else {
    if (inherits(x, "antsImage")){
      img = x
    } else {
      stop("x has unknown class - not char or antsImage")
    }
  }
  return(img)
}

