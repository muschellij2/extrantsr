#' @title Copy Information of One Image to Another
#' @description Wraps \code{antsSetOrigin/antsSetDirection/antsSetSpacing} 
#' and \code{antsGetOrigin/antsGetDirection/antsGetSpacing}
#' to copy the information over.  Finer control than 
#' \code{antsCopyImageInfo}
#'
#' @param reference \code{antsImage} object to get origin values from
#' @param target \code{antsImage} object to copy origin values to
#'
#' @return \code{target} object of class \code{antsImage}
#' @export
#' @name antsCopy
#' @rdname antsCopy
#' @examples
#' library(ANTsR)
#' img <- makeImage(c(10,10),rnorm(100))
#' img2 <- makeImage(c(10,10), rnorm(100))
#' img2 <- antsCopyOrigin(img, img2)
antsCopyOrigin = function(reference, target){
  antsSetOrigin(target, as.numeric(antsGetOrigin(reference)))
  return(target)
}


#' @rdname antsCopy
#' @export
antsCopyDirection = function(reference, target){
  antsSetDirection(target, antsGetDirection(reference))
  return(target)
}

#' @rdname antsCopy
#' @export
antsCopySpacing = function(reference, target){
  antsSetSpacing(target, antsGetSpacing(reference))
  return(target)
}

