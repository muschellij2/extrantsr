
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
#' @import ANTsR
#' @return Output from \code{\link{antsImageWrite}} 
c3d <- function(
  infile, # input filename
  outfile, # output filename with extension
  ... # arguments to be passed to \code{\link{antsImageRead}}
  ){
  img = antsImageRead(filename = infile, ...)
  antsImageWrite(img, filename = outfile)
}