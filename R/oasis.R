#' @title OASIS Processing Pipeline
#'
#' @description This function performs registration to a T1 template
#' using ANTsR transformation
#' @param filename filename of T1 image
#' @param skull_strip do skull stripping with FSL BET 
#' @param skull_stripfile Output skull strip filename
#' @param correct do N3 Bias correction
#' @param normalize Normalize data using \code{\link{whitestripe}}
#' @param normalize_file \code{\link{whitestripe}} image mask
#' @param retimg return a nifti object from function
#' @param outfile output filename should have .nii or .nii.gz 
#' extension
#' @param template.file Filename of template to warp to
#' @param interpolator interpolation done for 
#' \code{\link{antsApplyTransforms}}
#' @param other.files Filenames of other iamges to be 
#' transformed with the T1
#' @param other.outfiles Output filenames of \code{other.files} to 
#' be written
#' @param typeofTransform type of transformed used, passed to 
#' \code{\link{antsRegistration}}
#' @param remove.warp (logical) Should warping images be deleted?
#' @param outprefix Character path of where the warp files should be stored.
#' Required if \code{remove.warp = FALSE}
#' @param bet.opts Options passed to \code{\link{fslbet}}
#' @param betcmd BET command used, passed to \code{\link{fslbet}}
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
#' @param ... arguments to \code{\link{whitestripe}}
#' @export
#' @return NULL or object of class nifti for transformed T1 image
#' @importFrom fslr fslbet get.imgext
#' @importFrom WhiteStripe whitestripe_norm
oasis <- function(
  filename, # filename of T1 image
  skull_strip = TRUE, # do Skull stripping with FSL BET
  skull_stripfile = NULL,
  correct = TRUE,  # do N3 Bias correction
  normalize = TRUE, # whitestripe normalization
  normalize_file = NULL,
  retimg = TRUE, # return a nifti object from function
  outfile = NULL, # output filename, should have .nii or 
  #.nii.gz extension
  template.file = file.path(fsldir(), "data", "standard", 
                            "MNI152_T1_1mm_brain.nii.gz"),
  interpolator = "LanczosWindowedSinc",
  other.files = NULL,
  other.outfiles= NULL,
  typeofTransform = "Rigid",
  remove.warp = TRUE,
  outprefix = NULL,
  bet.opts = "-B -f 0.1 -v",
  betcmd = "bet",
  reproducible = TRUE,
  seed = 1,      
  ... # arguments to \code{\link{antsApplyTransforms}} 
){
  
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
  
  #### setup files
  writeFile = FALSE
  if (retimg){
    if (is.null(outfile)) {
      outfile = paste0(tempfile(), ".nii.gz")
    } else {
      writeFile = TRUE
    }
  } else {
    stopifnot(!is.null(outfile))
  }	
  ### check if other files are there
  have.other = FALSE
  if (!is.null(other.files)){
    have.other = TRUE
    lother = length(other.files)
    lout = length(other.outfiles)
    if (lother != lout) {
      stopifnot("Other outfile and infiles must be same length")
    }
  }
  
  #### should warp be removed
  if (!remove.warp){
    stopifnot(!is.null(outprefix))
  } else {
    outprefix = tempfile()
  }
  stopifnot(file.exists(filename))
  stopifnot(file.exists(template.file))
  t1 <- antsImageRead(filename, 3)
  
  #### Run BET
  if (skull_strip){
    ext = get.imgext()
    bet_file = tempfile()
    fslbet(infile = filename, 
           outfile = bet_file, 
           opts = bet.opts, 
           betcmd = betcmd, retimg= FALSE)
    bet_maskfile = paste0(bet_file, "_mask", ext)
    bet_file = paste0(bet_file, ext)
    bet = antsImageRead(bet_file, 3)
    bet_mask = antsImageRead(bet_maskfile, 3)
    if (!is.null(skull_stripfile)){
      file.copy(bet_maskfile, skull_stripfile, overwrite = TRUE)
    }
  }
  
  t1N3 <- antsImageClone(t1)
  
  #### Get Other Files
  if (have.other) {
    other.exists = sapply(other.files, file.exists)
    stopifnot(all(other.exists))
    other.imgs = lapply(other.files, antsImageRead, 
                        dimension = 3)
    N3.oimgs = lapply(other.imgs, antsImageClone)
  }
  ## N3 Correct files
  if (correct){
    t1N3 = n3BiasFieldCorrection(t1, 4)
    if (have.other) {
      for (i in seq(lother)){
        N3.oimgs[[i]] = n3BiasFieldCorrection(other.imgs[[i]], 4)
      }
    }		
  }
  
  t1N3_bet <- antsImageClone(t1N3)
  
  #### Mask the files by brain
  if (skull_strip){
    t1N3_bet = maskImage(t1N3, bet_mask)
    if (have.other) {
      for (i in seq(lother)){
        N3.oimgs[[i]] = maskImage(N3.oimgs[[i]], bet_mask) 
      }
    }
  }
  
  ## ## read in Template
  template <- antsImageRead(template.file, 3)
  # template.img <- readnii(template.path, reorient = FALSE)
  
  ### T1 TO TEMPLATE
  antsRegOut.nonlin <- antsRegistration(
    fixed = template, 
    moving = t1N3_bet, 
    typeofTransform = typeofTransform,  
    outprefix = outprefix)
  
  ### T1 TO TEMPLATE Write  - with skull
  t1.to.template <- antsApplyTransforms(fixed=template, 
                                        moving=t1N3_bet,
                                        transformlist=antsRegOut.nonlin$fwdtransforms,
                                        interpolator=interpolator) 
  
  
  output = paste0(tempfile(), ".nii.gz")
  
  if (have.other) {
    reg.oimgs = lapply(N3.oimgs, function(x){
      tprefix = tempfile()
      antsRegOut.temp <- antsRegistration(
        fixed = t1.to.template, 
        moving = x, 
        typeofTransform = typeofTransform,  
        outprefix = tprefix)           
      xx = antsApplyTransforms(fixed=t1.to.template, 
                               moving = x, 
                               transformlist=antsRegOut.temp$fwdtransforms,
                               interpolator=interpolator)
      xx
    })
  }
  
  antsImageWrite(t1.to.template, outfile)
  
  #### Run BET
  #   if (skull_strip){
  #     ext = get.imgext()
  #     bet_file = tempfile()
  #     fslbet(infile = outfile, 
  #                outfile = bet_file, 
  #                opts = bet.opts, 
  #                betcmd = betcmd, retimg= FALSE)
  #     bet_maskfile = paste0(bet_file, "_mask", ext)
  #     bet_file = paste0(bet_file, ext)
  #     bet = antsImageRead(bet_file, 3)
  #     bet_mask = antsImageRead(bet_maskfile, 3)
  #     if (!is.null(skull_stripfile)){
  #       file.copy(bet_maskfile, skull_stripfile, overwrite = TRUE)
  #     }
  #     #### Mask the files by brain
  #     t1.to.template = maskImage(t1.to.template, bet_mask)
  #     antsImageWrite(t1.to.template, outfile)
  #     
  #     if (have.other) {
  #       for (i in seq(lother)){
  #         reg.oimgs[[i]] = maskImage(reg.oimgs[[i]], bet_mask)
  #       }
  #     } 
  #   }
  
  if (have.other) {
    for (i in seq(lother)){
      antsImageWrite(reg.oimgs[[i]], 
                     other.outfiles[i])
    }
  }
  
  if (remove.warp){
    files = unlist(antsRegOut.nonlin[
      c("fwdtransforms", "invtransforms")])
    files = grep("Warp", files, value=TRUE)
    file.remove(files)
  }
  
  ###########################################
  # White Stripe Normalization
  ###########################################
  img = readnii(outfile, reorient= FALSE)
  if (normalize) {
    ws = whitestripe(img, type = "T1", ...)
    mask.img = ws$mask.img
    if (!is.null(normalize_file)){
      normalize_file = nii.stub(normalize_file)
      writeNIfTI(ws$mask.img, filename = normalize_file)
    }
    ### need to mask - so outside isn't changed
    mask = niftiarr(img, img != 0)
    img[ mask == 0 ] = NA
    img = whitestripe_norm(img, indices = ws$whitestripe.ind, na.rm=TRUE)
    img[ mask == 0] = 0
    writeNIfTI(img, filename = nii.stub(outfile))
    if (have.other) {
      for (i in seq(lother)){
        oimg = readnii(other.outfiles[i], reorient= FALSE)
        oimg[ mask == 0 ] = NA
        oimg = whitestripe_norm(oimg, 
                                indices = ws$whitestripe.ind, 
                                na.rm=TRUE)
        oimg[ mask == 0] = 0
        writeNIfTI(oimg, filename = nii.stub(other.outfiles[i]))
      }
    }
  }
  
  if (retimg){
    return(img)
  } else{
    return(invisible(NULL))
  }
}


