
#' @title Plot Lightbox for antsImage 
#'
#' @description Plots series of Images for slices of antsImage
#' @param img Image to be plotted
#' @param oimg image to be overlaid
#' @param slices slice to be displayed
#' @param col.x grayscale coloring for img
#' @param col.y color for overlay
#' @param ... arguments to be passed to \code{\link[graphics]{image} }
#' @export
#' @return NULL
ants.overlay <- function(
  img, # Image to be plotted
  oimg, # image to be overlaid
  slices = 53, #  slice to be displayed
  col.x = gray(0:64/64), # grayscale coloring for img
  col.y= "#FF0000FF", # color for overlay
  ... # arguments to be passed to \code{\link[graphics]{image})
){
  oldpar <- par(no.readonly = TRUE)
  lslice = length(slices)
  par(mfrow = c(1,lslice),
      mar = rep(0, 4), 
      bg = "black")
  for (islice in seq(lslice)){
    slice = slices[islice]
    image(as.array(img)[,,slice], col = col.x, ...)
    oslice = as.array(oimg)[,,slice]
    oslice[oslice == 0 ] = NA
    image(oslice, 
          col = col.y,
          add=TRUE)
  }
  par(oldpar)
  invisible()
}    



