#' @docType methods
#' @rdname ortho2-methods
#' @aliases ortho2,antsImage,ANY,ANY-method
#' @title Orthographic Display, added options
#' @description Wrapper for of \code{neurobase}'s \code{\link{ortho2}} function 
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
#' @param useRaster logical; if TRUE a bitmap raster is used to 
#' plot the image instead of polygons.  Passed to 
#' \code{\link[graphics]{image}}.
#' @param ... other arguments to the image function may be provided here.
#' @param mask If a mask is passed, \code{drop_empty_dim} is applied 
#' to both \code{x} and \code{y}
#' @note This will NOT do \code{\link{ants2oro}} to plot the image.  
#' If that is desired, users will have to do that before calling 
#' \code{ortho2}.
#' @export
#' @importFrom neurobase ortho2
#' @importFrom oro.nifti hotmetal
setMethod("ortho2", 
          c(x = "antsImage", y = "ANY", pdim = "ANY"), 
          function(
            x, y = NULL,
            xyz = NULL, w = 1, col = gray(0:64/64), 
            col.y = hotmetal(), zlim = NULL, zlim.y = NULL, 
            NA.x = FALSE,
            NA.y = TRUE,
            crosshairs = TRUE, 
            col.crosshairs = "red", xlab = "", ylab = "", axes = FALSE, 
            oma = c(0, 0, 0, ifelse(ycolorbar, 5, 0)), 
            mar = rep(0, 4), bg = "black", text = NULL, 
            text.color = "white", text.cex = 2, 
            text.x=32,
            text.y=32,
            add.orient=TRUE,
            mfrow=c(2,2), ybreaks = NULL, breaks=NULL,
            addlegend = FALSE,
            leg.x=32,
            leg.y=32,
            legend,
            leg.col,
            leg.title = NULL,
            leg.cex,
            window=NULL,
            ycolorbar = FALSE,
            clabels = TRUE,
            add = TRUE,
            pdim = NULL,             
            useRaster = TRUE,
            mask = NULL, 
            ...) {
            
            if (is.antsImage(x)) {
              pdim = c(1, antsGetSpacing(x))
              x = as.array(x)
            }
            if (!is.null(y)) {
              if (is.antsImage(y)) {
                y = as.array(y)
              }
            }
            neurobase::ortho2(x = x, y = y, 
                              pdim = pdim, 
                              xyz = xyz,
                              w = w,
                              col = col,
                              col.y = col.y,
                              zlim = zlim,
                              zlim.y = zlim.y,
                              NA.x = NA.x,
                              NA.y = NA.y,
                              crosshairs = crosshairs,
                              col.crosshairs = col.crosshairs,
                              xlab = xlab,
                              ylab = ylab,
                              axes = axes,
                              oma = oma,
                              mar = mar,
                              bg = bg,
                              text = text,
                              text.color = text.color,
                              text.cex = text.cex,
                              text.x = text.x,
                              text.y = text.y,
                              add.orient = add.orient,
                              mfrow = mfrow,
                              ybreaks = ybreaks,
                              breaks = breaks,
                              addlegend = addlegend,
                              leg.x = leg.x,
                              leg.y = leg.y,
                              legend = legend,
                              leg.col = leg.col,
                              leg.title = leg.title,
                              leg.cex = leg.cex,
                              window = window,
                              ycolorbar = ycolorbar,
                              clabels = clabels,
                              add = add,
                              useRaster = useRaster,
                              mask = mask,
                              ... = ...)
          }
)

#' @export
#' @rdname ortho2-methods
#' @rdname ortho2
#' @aliases ortho2,niftiImage,ANY,ANY-method
setMethod(
  "ortho2", 
  c(x = "niftiImage", y = "ANY", pdim = "ANY"),
  function(
    x, y = NULL,
    ....,
    pdim = NULL,             
    ...) {
    
    if (is.null(pdim)) {
      pdim = c(1, RNifti::pixdim(x))
      if (length(pdim) <= 3) {
        pdim = c(pdim, rep(1, 4 - length(pdim)))
      }
    }
    neurobase::ortho2(x = x, y = y, 
           pdim = pdim, 
           ... = ...)
  }
)