#' @title Run Atropos for nifti objects
#'
#' @description Runs Atropos (a finite mixture model) but with more 
#' robust inputs
#' @param a Images to segment.  Can be a \code{nifti} object, 
#' \code{antsImage} object, \code{character} vector, or list of these
#' @param x Mask image.  If none is supplied and \code{make_mask = TRUE},
#' then mask is created by first 
#' @param m mrf parameters as a string, usually 
#' "\code{[smoothingFactor,radius]}".  Reset for 3D images, compared to 
#' 2D default atropos has. 
#' @param verbose print diagnostic messages
#' @param ... Additional arguments to be passed to \code{\link{atropos}}
#' @param make_mask Logical indicating if mask should be created if
#' \code{x} is missing.  Mask is made using \code{\link{getMask}} with
#' first image of \code{a}.
#' @param reset_origin Should \code{\link{antsCopyOrigin}} be done
#' on the mask?
#' @export
#' @return Result of \code{\link{atropos}}, but with nifti objects.
otropos <- function(a, 
                    x, 
                    m = "[0.2,1x1x1]", 
                    verbose = TRUE,
                    ..., 
                    make_mask = TRUE,
                    reset_origin = TRUE) {
  if ( is.list(a) | (is.character(a) & length(a) > 0)  ) {
    a = lapply(a, check_ants)
    img = antsImageClone(a[[1]])
  } else {
    a = check_ants(a)
    img = antsImageClone(a)
  }
  
  if (missing(x)) {
    if (make_mask) {
      x = getMask(img)
      args = list(x = x, ...)
    } else {
      args = list(...)
    }
  } else {
    x = check_ants(x)
    x = antsImageClone(
      (x * 0) + (x > 0), 
      out_pixeltype = "unsigned int")
    if (reset_origin) {
      x = antsCopyOrigin(img, x)
    }
    args = list(x = x, ...)
  }
  verbose = as.numeric(verbose)
  args = c(a = img, args, m = m)
  args$v = NULL
  args$v = verbose
  
  res = do.call(atropos, args)
  rm(list = c("img", "args")); gc(); gc();
  if (!all(c("segmentation", "probabilityimages") %in% names(res))) {
    warning(paste0("Results have non-standard output, cowardly ",
                   "returning direct result of res"))
  } else {
    ants_out_seg = ants2oro(res$segmentation)
    ants_out_seg = datatyper(ants_out_seg)
    res$segmentation = ants_out_seg
    rm(list = "ants_out_seg"); gc();
    for (i in seq_along( res$probabilityimages)) {
      tmp = res$probabilityimages[[i]] 
      tmp = ants2oro(tmp)
      res$probabilityimages[[i]] = tmp
      rm(list = "tmp"); gc();
    } 
    gc();
  }
  return(res)
}

#' @title Run Atropos for nifti objects
#'
#' @description Runs \code{\link{otropos}} but has different argument
#' names to be more consistent with \code{extrantsr}
#' @param img Images to segment.
#' @param mask Mask of Image
#' @param smoothing_factor amount of smoothing in Markov random Field. 
#' Increasing this number causes more smoothing whereas decreasing 
#' the number lessens the smoothing.
#' @param radius the mrf neighborhood.  Length must equal the number
#' of dimensions of \code{img}
#' @param ... arguments to pass to \code{\link{otropos}}
#' @seealso \code{\link{otropos}}, \code{\link{atropos}}
#' @export
otropos2 <- function(
  img, 
  mask = NULL,
  smoothing_factor = 0.2,
  radius = c(1,1,1),
  ...) {
  
  if ( is.list(img) | (is.character(img) & length(img) > 0)  ) {
    img = lapply(img, check_ants)
    img = antsImageClone(img[[1]])
  } else {
    img = check_ants(img)
    img = antsImageClone(img)
  }  
  dimg = dim(img)
  ndim = length(dimg)
  if (length(radius) != ndim) {
    stop(paste0("Length of radius not the same as number", 
                " of dimensions of img"))
  }
  m = paste0("[", smoothing_factor, ",", 
             paste(radius, collapse = "x"),
             "]")
  L = list(...)
  nL = names(L)
  if (c("a", "m", "x") %in% nL) {
    stop("cannot specify a, m, or x in otropos2")
  }
  args = list(a = img, m = m, ...)
  args$x = mask
  
  res = do.call(otropos, args)
  return(res)
}


