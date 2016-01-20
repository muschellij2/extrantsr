#' @export
setMethod(f = "checkimg", signature(file = "antsImage"), definition = function(file, ...) { 
  file = tempants(file, ...)
  return(file)
})