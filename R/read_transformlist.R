#' @title Read Transform List from ANTsR
#' @description Transform a transformlist from ANTsR into an object in R
#' 
#' @param transformlist Character vector of transform list
#' @param useOro Use \code{oro.nifti} (if \code{TRUE}) or ANTs
#' @return List of transforms
#' @export
#' @importFrom R.matlab readMat
read_transformlist = function(transformlist, 
                              useOro = TRUE){
  endings = gsub("(.*)[.](.*$)", "\\2", transformlist)
  type = rep(NA, length = length(transformlist))
  type[ endings %in% 
          c("gz", ".gz",
            "nii", ".nii",
            "nii.gz", ".nii.gz")] = "image"
  type[ endings %in% 
          c("mat", ".mat",
            "txt", ".txt")] = "matrix"
  stopifnot( !any(is.na(type)))
  
  trans = mapply(function(xx, tt) {
    if (tt %in% "matrix") {
      return(R.matlab::readMat(xx))
    }
    if (tt %in% "image") {
      if (useOro) {
        img = fslr::readnii(xx,
                      drop_dim = FALSE)
      } else {
        img = antsImageRead(xx)
      }
      return(img)
    }        
  }, transformlist, type, 
  SIMPLIFY = FALSE)
  return(trans) 
}


#' @title Read Transform List from ANTsR
#' @description Transform a transformlist from ANTsR into an object in R
#' 
#' @param transformlist List of transform elements
#'
#' @return List of transforms
#' @export
#' @importFrom R.matlab writeMat
write_transformlist = function(transformlist){
  classes = sapply(transformlist, class)
  type = rep(NA, length = length(transformlist))
  type[ classes %in% c("nifti", "niftiExtension", 
                       "antsImage")] = "image"
  type[ classes %in% c("list")] = "matrix"
  stopifnot( !any(is.na(type)))
  
  trans = mapply(function(xx, tt){
    if (tt %in% "matrix") {
      fname = tempfile(fileext = ".txt")
      nn = names(xx)
      stopifnot(all(nn %in% c("AffineTransform.float.3.3", "fixed")))
      
      hdr = c("#Insight Transform File V1.0", "#Transform 0", 
              "Transform: AffineTransform_float_3_3")
      fmt = "%15.15f"
      params = paste0(sprintf(fmt, xx$AffineTransform.float.3.3), 
                      collapse = " ")
      hdr = c(hdr, paste0("Parameters: ", params))
      fixed = paste0(sprintf(fmt, xx$fixed), 
                      collapse = " ")      
      hdr = c(hdr, paste0("FixedParameters: ", params))
      writeLines(text = hdr, sep = "\n", con = fname)
    }
    if (tt %in% "image") {
      fname = tempfile(fileext = ".nii.gz")
      if (is.antsImage(xx)) {
        antsImageWrite(image = xx, filename = fname)
      } else {
        pd = pixdim(xx)
        pd[ pd == 0] = 1
        pixdim(xx) = pd
        writenii(xx,
                 filename = fname,
                 drop_dim = FALSE,
                 dtype = FALSE)
      }
    }        
    return(fname)
  }, transformlist, type, SIMPLIFY = TRUE)
  names(trans) = NULL
  return(trans) 
}