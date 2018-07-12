
#' Set reproducible seeds and threads for ANTsR execution
#'
#' @param seed will execute 
#' \code{Sys.setenv(ANTS_RANDOM_SEED = seed)} before
#' running to attempt a more reproducible result.
#' 
#' @note See
#' \url{https://github.com/ANTsX/ANTs/wiki/antsRegistration-reproducibility-issues}
#' for discussion.  
#'
#' @return A list of `ANTS_RANDOM_SEED` and `ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS`
#' @export
#'
#' @examples
#' reproducible_ants(1234)
reproducible_ants = function(seed = 1) {
  if (is.null(seed)) {
    stop("For reproducible = TRUE, you must set a seed!")
  }
  Sys.setenv(ANTS_RANDOM_SEED = seed)    
  Sys.setenv(ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS = 1)
  return(list(ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS = 1,
              ANTS_RANDOM_SEED = seed))
}