
#' @title Convert NeuroImage Types
#'
#' @description Simple wrapper for \code{\link{antsImageRead}} and 
#' \code{\link{antsImageWrite}} for conversion of image file types. 
#' For example, NRRD files with nhdr/raw file formats can be converted to
#' \code{.nii.gz}.
#' @param infile input filename
#' @param outfile output filename with extension
#' @param ... arguments to be passed to \code{\link{antsImageRead}}
#' @export
#' @return Output from \code{\link{antsImageWrite}} 
c3d <- function(
  infile, # input filename
  outfile, # output filename with extension
  ... # arguments to be passed to \code{\link{antsImageRead}}
  ){
  img = antsImageRead(filename = infile, ...)
  res = antsImageWrite(img, filename = outfile)
  rm(list = "img"); 
  for (i in 1:10) {
    gc()
  }
  return(res)
}