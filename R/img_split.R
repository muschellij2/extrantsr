#' @title Split an Image into Multiple Sub-Images
#' @description Split an image on a certain dimension
#' @param img Object of class \code{antsImage}
#' @param output_basename prefix for output files
#' @param verbose print diagnostic messages
#' @return Names of output files
#' @export
#' @importFrom R.utils extract.array
img_split = function(img, 
                     # direction = c("t", "x", "y", "z"),
                     output_basename = NULL,
                     verbose = TRUE
                     ){
  stop("work in progress")
  img = check_ants(img)
  arr = as.array(img)
  
  # list = img_ts_to_list(arr, copy_nifti = FALSE)
  
  # direction = match.arg(direction)
  direction = "t"
  levs = c("x", "y", "z", "t")
  indexer = which( levs %in% direction)
  all_dims = dim(img)
  L = lapply(all_dims, seq)  
  
  if (direction != "t") {
    stop("Not implemented yet for other dimensions")
  }
  stopifnot(length(all_dims) == 4)
  names(L) = levs[seq(length(L))]

  go_over = L[[direction]]
  n_values = length(go_over)
  if (is.null(output_basename)) {
    output_basename = tempfile(fileext = "_")
  }
  endings = sprintf("%04.0f.nii.gz", seq(0, n_values - 1))
  outfiles = paste0(output_basename, endings)

  map = switch(direction,
               "t" = "l",
               "x" = "i",
               "y" = "j",
               "z" = "k")
  
  ival = 1
  for (ival in seq(n_values)){
    slice = R.utils::extract.array(arr, 
                                   indices = ival,
                                   dims = indexer,
                                   drop = FALSE)
      oimg = oro2ants(slice, reference = img)
      outfile = outfiles[ival]
      if (verbose){
        message(paste0("Writing file ", ival, " of ", n_values))
      }
      antsImageWrite(image = oimg, filename = outfile)
  }
  
  return(outfiles)

}