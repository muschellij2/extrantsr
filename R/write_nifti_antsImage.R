#' @export
#' @method write_nifti antsImage
#' @importFrom neurobase write_nifti
write_nifti.antsImage = function(nim, 
                                  filename){
  ANTsRCore::antsImageWrite(image = nim, filename = filename)
}
