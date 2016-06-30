#' @title Neighborhood Correlation Image
#' @description 
#'
#' @param img1 image of class \code{nifti}, \code{antsImage}, \code{character}
#' @param img2 image of class \code{nifti}, \code{antsImage}, \code{character}
#' @param mask Binary image of class \code{nifti}, 
#' \code{antsImage}, \code{character}.  Only neighborhoods inside the mask 
#' will be taken
#' @param radius vector of length 3 for number of voxels to go in each direction.
#' Default is 27 neighbors (including voxel at center).  
#' Passed to \code{\link{getNeighborhoodInMask}}.
#' @param method Type of correlation.
#' @param verbose print diagnostic messages
#'
#' @return Object of class \code{nifti}
#' @export 
#'
#' @examples \dontrun{
#' corr_img("T1_Image.nii.gz", "FLAIR_Image.nii.gz", mask = "Mask.nii.gz")
#' }
corr_img = function(
  img1, 
  img2,
  mask = NULL, 
  radius = rep(1,3), 
  method = c("pearson", "spearman"),
  verbose = TRUE){
  
  method = match.arg(method)
  img1 = check_ants(img1)
  img2 = check_ants(img2)
  if (is.null(mask)) {
    mask = array(1, dim = dim(img1))
    mask = as.antsImage(mask)
    mask = antsCopyImageInfo(
      target = mask, 
      reference = img1)
  }
  xmask = mask
  
  mask = check_ants(mask)
  if (verbose) {
    message("Checking dimensions of images")
  }
  stopifnot(
    same_dims(img1, img2, mask)
  )
  
  col_scale = function(x){
    cm = colMeans(x, na.rm = TRUE)
    csd = colSds(x, center = cm,
                 na.rm = TRUE)
    x = t( (t(x) - cm) / csd )
  }
  neigh = function(img, mask, radius, method){
    grads = getNeighborhoodInMask(
      image = img, 
      mask = mask, 
      radius = radius,
      spatial.info = TRUE)
    # not_zero = grads1$offsets != 0
    # neighbor = rowSums(not_zero) > 0
    if (method == "spearman") {
      grads$values = colRanks(grads$values, ties.method = "average")
    }
    grads$values = col_scale(
      grads$values)
    return(grads)
  }
  
  if (verbose) {
    message("Getting Neighborhood for Image 1")
  }  
  neigh1 = neigh(img = img1, 
                 mask = mask, 
                 radius = radius,
                 method = method)
  if (verbose) {
    message("Getting Neighborhood for Image 2")
  }    
  neigh2 = neigh(img = img2,
                 mask = mask, 
                 radius = radius,
                 method = method)
  
  stopifnot(
    identical(neigh1$indices,
              neigh2$indices)
  )  
  
  if (verbose) {
    message("Calculating Correlations")
  } 
  corrs = neigh1$values * neigh2$values
  nNA <- colCounts(corrs, 
                   value = NA_real_, 
                   na.rm = FALSE)
  n = nrow(corrs) - nNA  
  corrs = colSums(corrs, na.rm = TRUE)
  corrs = corrs / (n - 1)
  # divide by n - 1
  # corrs = colMeans(corrs, na.rm = TRUE)
  
  reorder_neighborhood = function(
    x, 
    img.dim){
    inds = x$indices + 1
    tmp = array(0, dim = img.dim)
    tmp[inds] = 1
    inds = which(tmp > 0)
    L = list(indices = inds,
             order_ind = order(inds),
             array = tmp)
  }
  #####################
  # Must ADD 1 because they are 0-indexed!
  #####################
  if (verbose) {
    message("Reordering indices from ANTsR output")
  } 
  out = reorder_neighborhood(neigh1,
                             img.dim = dim(img1))
  ord = out$order_ind
  corrs = corrs[ ord ]
  
  if (verbose) {
    message("Creating Output Image")
  }   
  xmask = check_nifti(xmask)
  ximg = xmask
  ximg[xmask == 1] = corrs
  ximg = cal_img(ximg)
  ximg = datatyper(ximg, 
                   datatype = 
                     convert.datatype()$FLOAT32,
                   bitpix = 
                     convert.bitpix()$FLOAT32)
  ximg   
}