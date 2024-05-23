
#' @importFrom ANTsR antsSetOrigin
#' @importFrom ANTsR antsSetDirection
#' @importFrom ANTsR antsSetSpacing
#' @importFrom ANTsR labelClusters
#' @importFrom ANTsR n3BiasFieldCorrection n4BiasFieldCorrection
#' @importFrom ANTsR antsImageRead antsImageWrite
#' @importFrom ANTsR check_ants
#' @importFrom ANTsR origin "origin<-"
#' @importFrom ANTsR antsImageWrite
#' @importFrom ANTsR antsImageMutualInformation
#' @importFrom ANTsR antsGetSpacing antsGetDirection antsGetOrigin
#' @importFrom ANTsR pixeltype components
#' @importFrom ANTsR antsCopyImageInfo getNeighborhoodInMask
#' @importFrom ANTsR getMask
#' @importFrom ANTsR iMath
#' @importFrom ANTsR antsCopyImageInfo as.antsImage antsImageRead
#' @importFrom ANTsR atropos
#' @importFrom ANTsR antsImageRead antsImageClone
#' @importFrom ANTsR resampleImage
#' @importFrom ANTsR smoothImage maskImage
#' @importFrom ANTsR as.antsImage
#' @importFrom ANTsR imageSimilarity
#' @importFrom ANTsR labelClusters
#' @importFrom ANTsR is.antsImage antsRegistration antsApplyTransforms 
#' @importFrom ANTsR resampleImageToTarget
#' 
#' @importFrom oro.nifti voxdim
#' @importFrom oro.nifti is.nifti
#' @importFrom oro.nifti cal_img writeNIfTI
#' 
#' @importFrom fslr fsldir fslmask
#' @importFrom fslr fslbet fsldir get.imgext
#' 
#' 
#' @importFrom WhiteStripe whitestripe whitestripe_hybrid whitestripe_norm
#' 
#' 
#' @importFrom neurobase write_nifti
#' @importFrom neurobase copyNIfTIHeader readnii mask_img check_outfile writenii datatyper
#' @importFrom neurobase zscore_img same_dims robust_window remake_img xyz zero_pad
#' @importFrom neurobase check_mask_fail niftiarr nii.stub
#' @importFrom neurobase window_img 
NULL