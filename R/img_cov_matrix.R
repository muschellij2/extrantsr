#' @title Neighborhood Covariance Matrix
#' @description Calculates the neighborhood of multiple images, and then
#' does the covariance of each neighbor with itself in the other imaging modality
#'
#' @param imgs list of images of class \code{nifti}, \code{antsImage}, \code{character}
#' @param mask Binary image of class \code{nifti}, 
#' \code{antsImage}, \code{character}.  Only neighborhoods inside the mask 
#' will be taken
#' @param radius vector of length 3 for number of voxels to go in each direction.
#' Default is 27 neighbors (including voxel at center).  
#' Passed to \code{\link{getNeighborhoodInMask}}.
#' @param method Type of correlation. If pearson, scaling done on original data.  
#' If spearman, the ranks. If you want a covariance, you simply need to do 
#' \code{scale = FALSE}
#' @param center Should the data be centered
#' @param scale Should the data be scaled 
#' @param verbose print diagnostic messages
#' @note Divisor is \code{n - 1}.
#' @return List of matrix of covariances for each voxel and the order the columns
#' are in.
#'
#' #' @export 
img_cov_matrix = function(
  imgs, 
  mask = NULL, 
  radius = rep(1,3), 
  method = c("pearson", "spearman"),
  center = TRUE,
  scale = FALSE,
  verbose = TRUE){
  
  method = match.arg(method)
  imgs = check_ants(imgs)

  if (is.null(mask)) {
    mask = array(1, dim = dim(imgs[[1]]))
    mask = as.antsImage(mask)
    mask = antsCopyImageInfo(
      target = mask, 
      reference = imgs[[1]])
  }
  xmask = mask
  
  mask = check_ants(mask)
  if (verbose) {
    message("Checking dimensions of images")
  }
  stopifnot(
    same_dims(list(imgs, mask))
  )
  
  if (verbose) {
    message("Getting Neighborhood for Images")
  }
  if (is.antsImage(imgs)) {
    stop("Need 2 or more images")
  }
  neighs = lapply(imgs, 
                  scaled_neighborhood,
                  mask = mask, 
                  radius = radius,
                  method = method, 
                  center = center,
                  scale = scale,
                  verbose = verbose)
  
  ind1 = neighs[[1]]$indices
  xx = lapply(neighs, function(x) {
    stopifnot(
      identical(x$indices,
                ind1)
    )
  })
  rm(list = "xx")
  
  if (verbose) {
    message("Calculating Correlations")
  } 
  n_imgs = names(imgs) # what if null!!
  if (is.null(n_imgs) || n_imgs != length(imgs)) {
    n_imgs = c("image_", seq(length(imgs)))
  }
  ord = t(combn(x = n_imgs, m = 2))
  diags =  t(sapply(n_imgs, rep, 2))
  ord = rbind(ord, diags)
  rownames(ord) = NULL
  ord = data.frame(ord, stringsAsFactors = FALSE)
  colnames(ord) = c("Var1", "Var2")
  # ord = arrange(ord, Var1, Var2)
  rm(list = "diags")
  ncombo = nrow(ord)

  iord = 1
  corr_mat = matrix(NA, 
                    nrow = nrow(ind1), 
                    ncol = ncombo)
  if (verbose) {
    pb = txtProgressBar(min = 0, max = nrow(ord), style = 3)
  }
  for (iord in seq(nrow(ord))) {
    ind_1 = ord[iord, 1]
    ind_2 = ord[iord, 2]
    val1 = neighs[[ind_1]]$values
    val2 = neighs[[ind_2]]$values
    corrs = val1 * val2
    nNA <- colCounts(corrs,
                     value = NA_real_,
                     na.rm = FALSE)
    n = nrow(corrs) - nNA
    corrs = colSums(corrs, na.rm = TRUE)
    corrs = corrs / (n - 1)
    corr_mat[, iord] = corrs
    if (verbose) {
      setTxtProgressBar(pb, value = iord)
    }
  }
  if (verbose) {
   close(pb)
  }
  rm(list = c("imgs", "neighs", "mask", "xmask"))
  for (i in 1:10) {
    gc()
  }  
  L = list(cov = corr_mat,
           ind = ord)
  return(L)
  
  # mat = matrix(NA, 
  #              nrow = length(n_imgs),
  #              ncol = length(n_imgs))
  # colnames(mat) = rownames(mat) = n_imgs
  # 
  # 
  # make_svd = function(vals){
  #   for (iord in seq(nrow(ord))) {
  #     ind_1 = ord[iord, 1]
  #     ind_2 = ord[iord, 2]
  #     mat[ind_1, ind_2] = vals[iord]
  #     mat[ind_2, ind_1] = vals[iord]
  #   }
  #   return(svd(mat, nu = 0))
  # }
  # 
  # svds = alply(corr_mat, 1, make_svd, .progress = "text")
  
  
  
 
}