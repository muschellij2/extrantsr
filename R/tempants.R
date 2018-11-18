#' @title Create temporary nii.gz file from antsImage
#'
#' @description   Takes in a object of class \code{antsImage},
#' writes it to a temp file, and appends .nii or .nii.gz
#' @param x object of class \code{antsImage}
#' @param gzipped logical if the tempfile should be gzipped or not
#' @export
#' @return Character value of filename
#' @importFrom neurobase tempimg
tempants <- function(x, # object of class \code{antsImage}
                     gzipped = TRUE # logical if the tempfile should be gzipped or not
){
  if (inherits(x, "character")) {
    return(x)
  } else {
    if (inherits(x, "antsImage")) {
      ext = ".nii"
      if (gzipped) ext = ".nii.gz"
      tfile = paste0(tempfile(), ext)
      antsImageWrite(x, tfile)
      rm( list = "x"); gc(); gc()
      return(tfile)
    } else if (inherits(x, "nifti")) {
      tfile = tempimg(x, gzipped = gzipped)
    } else {
      stop("x has unknown class - not char or nifti")
    }
  }
  return(FALSE)
}


