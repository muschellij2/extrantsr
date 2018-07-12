#' @title Wrapper for ANTs brain extraction
#' @description This function wraps \code{\link{abpN4}} and
#' \code{\link{abpBrainExtraction}} for \code{nifti} images with additional checks
#' 
#'
#' @param img Image for brain extraction, nifti, character or antsImage
#' @param template Template image with mask, nifti, character or antsImage
#' @param template.mask Template brain mask
#' @param typeofTransform registration type: 'SyNabp' (better, slower), 
#' 'SyN' (fast)
#' @param correct Should \code{\link{abpN4}} be performed first?
#' @param reproducible Sets the seed and 
#' \code{Sys.setenv(ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS = 1)}.
#'  See
#' \url{https://github.com/ANTsX/ANTs/wiki/antsRegistration-reproducibility-issues}
#' for discussion.
#' @param seed will execute 
#' \code{Sys.setenv(ANTS_RANDOM_SEED = seed)} before
#' running to attempt a more reproducible result.   If \code{NULL}, will not set anything, 
#' but \code{reproducible} must be \code{FALSE}.    
#' 
#' @param ... additional arguments passed to \code{\link{abpBrainExtraction}}
#'
#' @return A list of the brain image, brain mask, k-means segmentation,
#' and transformations.
#' @export
#' @importFrom ANTsR abpBrainExtraction
brain_extract = function(
  img, template, template.mask,
  typeofTransform = c("SyNabp", "SyN"), correct = TRUE, 
  reproducible = TRUE,
  seed = 1,      
  ...) {
  
  if (reproducible) {
    if (is.null(seed)) {
      stop("For reproducible = TRUE, you must set a seed!")
    }
    Sys.setenv(ANTS_RANDOM_SEED = seed)    
    itk_threads = Sys.getenv("ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS")
    Sys.setenv(ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS = 1)
    on.exit({
      Sys.setenv(ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS = itk_threads)
    })
  }   
  img = check_ants(img)
  if (correct) {
    img = ANTsR::abpN4(img = img)
  }
  template = check_ants(template)
  template.mask = check_ants(template.mask)
  typeofTransform = match.arg(typeofTransform)
  res = ANTsR::abpBrainExtraction(
    img = img, tem = template, 
    temmask = template.mask, 
    regtype = typeofTransform, ...)
  res$brain = ants2oro(res$brain)
  res$bmask = ants2oro(res$bmask, reference = res$brain)
  res$kmeansseg = ants2oro(res$kmeansseg, reference = res$brain)
  return(res)
}
