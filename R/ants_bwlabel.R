#' @title ANTs BWLabel Clusters of Certain Size
#'
#' @description Get Cluster of certain size from \code{\link{labelClusters}}
#' @param img input image
#' @param k Minimum cluster size needed
#' @param binary (logical) Should the result be binary or numbered with cluster.
#' @return \code{nifti} object
#' @importFrom ANTsR labelClusters
#' @export
ants_bwlabel = function(img, k = 1, binary = TRUE) {
  
  img = check_ants(img)
  labs = labelClusters(
    img,
    minClusterSize = 1,
    fullyConnected = TRUE)
  tab = table(c(as.array(labs)))
  levs = names(tab[tab >= k])
  levs = as.numeric(levs)
  levs = levs[ levs > 0 ]
  olabs = ants2oro(labs)
  yhat = niftiarr(olabs, olabs %in% levs)
  if (!binary){
    yhat = mask_img(olabs, yhat)
  }
  rm(list = c("labs", "img", "tab", "levs", "olabs"))
  for (i in 1:10){
    gc();
  }
  return(yhat)
  
}