#' @export
#' @method write_nifti antsImage
write_nifti.antsImage = function(nim, 
                                 filename, ...){
  antsImageWrite(image = nim, filename = filename)
}
