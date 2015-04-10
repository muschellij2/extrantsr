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
  if (is.character(x)) {
    img = antsImageRead(x, dimension = dimension)
    return(img)
  } 
  if (is.nifti(x)) {
    img = oro2ants(x)
    return(img)
  }
  if (is.antsImage(x)) {
    return(x)
  }  
  msg = paste0("x has class ", class(x), " - not char, nifti, antsImage")
  stop(msg)
  return(NULL)
}

