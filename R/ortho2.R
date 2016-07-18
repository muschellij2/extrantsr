#' @docType methods
#' @rdname ortho2-methods
#' @aliases ortho2,antsImage,ANY,ANY-method
#' @title Orthographic Display, added options
#' @description Wrapper for of \code{fslr}'s \code{\link{ortho2}} function 
#' to work with antsImages
#' @return NULL
#' @param x is an object of class \code{antsImage} or similar.
#' @param y is an object of class \code{antsImage} or similar for the overlay.
#' @param pdim Pixel dimensions if passing in arrays.  
#' @param xyz is the coordinate for the center of the crosshairs.
#' @param w is the time point to be displayed (4D arrays only).
#' @param col is grayscale (by default).
#' @param col.y is hotmetal (by default).
#' @param zlim is the minimum and maximum `z' values passed into image.
#' @param zlim.y is the minimum and maximum `z' values passed into image 
#' for the overlay.
#' @param crosshairs is a logical value for the presence of crosshairs 
#' in all three orthogonal planes (default = TRUE).
#' @param NA.x Set any values of 0 in \code{x} to \code{NA}
#' @param NA.y Set any values of 0 in \code{y} to \code{NA}
#' @param col.crosshairs is the color of the crosshairs (default = red).
#' @param xlab is set to "" since all margins are set to zero.
#' @param ylab is set to "" since all margins are set to zero.
#' @param axes is set to FALSE since all margins are set to zero.
#' @param oma is the size of the outer margins in the par function.
#' @param mar is the number of lines of margin in the par function.
#' @param bg is the background color in the par function.
#' @param text allows the user to specify text to appear in 
#' the fourth (unused) pane.
#' @param text.color is the color of the user-specified text 
#' (default = ``white").
#' @param text.cex is the size of the user-specified text (default = 2).
#' @param text.x x coordinate for text 
#' @param text.y y coordinate for text
#' @param add.orient (logical) Add left/right, A/P, etc. orientation
#' @param mfrow (numeric) layout of the 3 slices
#' @param breaks (numeric) breaks for x to passed to 
#' \code{\link[graphics]{image}}
#' @param ybreaks (numeric) breaks for y to passed to 
#' \code{\link[graphics]{image}}
#' @param addlegend (logical) add legend?
#' @param leg.x (numeric) x coord for legend
#' @param leg.y (numeric) y coord for legend
#' @param legend (character) legend text
#' @param leg.col (character) Colors for legend 
#' @param leg.title (character) title for legend 
#' @param leg.cex (numeric) \code{cex} for \code{\link{legend}}
#' @param window (vector) Length-2 vector to limit image to certain range
#' @param ycolorbar (logical) Should a colorbar for \code{y} be plotted
#' @param clabels Label for colorbar (see \code{\link{colorbar}})
#' @param add Should the y-plot be added or its own plot?  Used
#' in \code{double_ortho}
#' @param ... other arguments to the image function may be provided here.
#' @note This will NOT do \code{\link{ants2oro}} to plot the image.  
#' If that is desired, users will have to do that before calling 
#' \code{ortho2}.
#' @export
setMethod("ortho2", c(x = "antsImage", 
                                y = "ANY",
                                pdim = "ANY"), 
          function(x, y = NULL, pdim = NULL, ...) {
  if (is.antsImage(x)) {
    pdim = c(1, antsGetSpacing(x))
    x = as.array(x)
  }
  if (!is.null(y)) {
    if (is.antsImage(y)) {
      y = as.array(y)
    }
  }
  fslr::ortho2(x = x, y = y, pdim = pdim, ...)
}
)