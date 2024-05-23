#' @title ANTs BWLabel Clusters of Certain Size
#'
#' @description Get Cluster of certain size from \code{\link{labelClusters}}
#' @param img input image
#' @param k Minimum cluster size needed
#' @param binary (logical) Should the result be binary or numbered with cluster.
#' @return \code{nifti} object
#' @export
ants_bwlabel = function(img, k = 1, binary = TRUE) {
  
  labs = label_clusters(
    img,     
    minClusterSize = 1,
    fullyConnected = TRUE, 
    retfile = TRUE)
  # CHANGE WHEN 0 isnt' background anymore!!!
  labs = labs - min(labs)
  tab = table(c(as.array(labs)))
  levs = names(tab[tab >= k])
  levs = as.numeric(levs)
  levs = levs[ levs > 0 ]
  olabs = ants2oro(labs)
  yhat = niftiarr(olabs, olabs %in% levs)
  if (!binary) {
    yhat = mask_img(olabs, yhat)
  }
  rm(list = c("labs", "img", "tab", "levs", "olabs"))
  for (i in 1:10) {
    gc();
  }
  return(yhat)
  
}

#' Label Clusters
#' 
#' @param img input image
#' @param retfile logical to indicate if an \code{antsImage} should be returned
#' \code{TRUE} (useful for chaining) or a \code{nifti} image
#' @param ... additional arguments passed to \code{\link{labelClusters}}
#' 
#' @return A \code{antsImage} or a \code{nifti} image, depending on
#' \code{retfile}
#' @export
label_clusters = function(img, ..., retfile = FALSE) {
  img = check_ants(img)
  labs = labelClusters(
    imagein = img,
    ...)
  if (!retfile) {
    labs = ants2oro(labs)
  }
  return(labs)
}

