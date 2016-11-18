#' @title Registration Wrapper function
#'
#' @description This function performs registration 
#' using ANTsR, carries out the transformation
#' on other images, and can back-transform image
#' in registered space to the native space of the original image.  Returns the 
#' transforms
#' @param filename filename of image to be registred
#' @param skull_strip do skull stripping with FSL BET 
#' @param correct do Bias correction
#' @param correction N3 or N4 correction, see \code{\link{bias_correct}}
#' @param retimg return a nifti object from function
#' @param outfile output filename should have .nii or .nii.gz 
#' extension
#' @param template.file Filename of image to warp to
#' @param interpolator interpolation done for 
#' \code{\link{antsApplyTransforms}}
#' @param other.files Filenames of other iamges to be 
#' transformed with the T1
#' @param other.outfiles Output filenames of \code{other.files} to 
#' be written
#' @param other.init Initial transformation lists (same length as \code{other.files})
#' to use in before the estimated transformation for one interpolation
#' @param template_to_native Logical indicating if native cerebellum should be 
#' created to \code{invert.native.fname}
#' @param invert.native.fname filename of native cerebellum file
#' @param invert.file Filename of image to invert to native space
#' @param typeofTransform type of transformed used, passed to 
#' \code{\link{antsRegistration}}
#' @param remove.warp (logical) Should warping images be deleted?
#' @param outprefix Character path of where the warp files should be stored.
#' Required if \code{remove.warp = FALSE}
#' @param bet.opts Options passed to \code{\link{fslbet}}
#' @param betcmd BET command used, passed to \code{\link{fslbet}}
#' @param copy_origin Copy image origin from t1, using \code{\link{antsCopyOrigin}}
#' @param verbose Print diagnostic messages
#' @param ... arguments to \code{\link{antsRegistration}}
#' @import ANTsR
#' @import fslr
#' @import oro.nifti
#' @export
#' @return List of the output filenames and transformations
registration <- function(filename, 
                         skull_strip = FALSE,
                         correct = FALSE,  
                         correction = "N4",
                         retimg = TRUE, 
                         outfile = NULL, 
                         template.file = file.path(
                           fsldir(), "data", 
                           "standard", 
                           "MNI152_T1_1mm_brain.nii.gz"),
                         interpolator="Linear", 
                         other.files = NULL,
                         other.outfiles = NULL,
                         other.init = NULL,
                         template_to_native = FALSE,
                         invert.native.fname = NULL,
                         invert.file = NULL,
                         typeofTransform = "SyN",
                         remove.warp = TRUE,
                         outprefix = NULL,
                         bet.opts = "-B -f 0.1 -v",
                         betcmd = "bet",
                         copy_origin = TRUE,
                         verbose = TRUE,
                         ... 
){
  
  outfile = check_outfile(outfile = outfile, retimg = retimg)
  
  have.other = FALSE
  if (!is.null(other.files)) {
    have.other = TRUE
    other.files = checkimg(other.files)
    other.outfiles = checkimg(other.outfiles)
    
    lother = length(other.files)
    lout = length(other.outfiles)
    if (lother != lout) {
      stop("Other outfile and infiles must be same length")
    }
    if (!is.null(other.init)) {
      ltrans = length(other.init)
      if (ltrans != lout) {
        stop("Other initial transformations and infiles must be same length")
      }    
    }
  }
  
  if (template_to_native) {
    stopifnot(!is.null(invert.native.fname))
    invert.file = checkimg(invert.file)
    if (length(invert.file) != length(invert.native.fname)) {
      stop("Length of invert.file and invert.native.fnames must be equal!")
    }
  }
  
  if (!remove.warp) {
    # stopifnot(!is.null(outprefix))
  } else {
    if (is.null(outprefix))  outprefix = tempfile()
  }
  
  if (is.antsImage(filename)) {
    t1 = antsImageClone(filename)
  } else {
    filename = checkimg(filename)
    # 	filename = path.expand(filename)
    stopifnot(file.exists(filename))
    t1 <- antsImageRead(filename, 3)
  }
  
  if (skull_strip) {
    if (verbose) {
      message("# Skull Stripping\n")
    }
    if (is.antsImage(filename)) {
      filename = checkimg(filename)
    }   
    ext = get.imgext()
    bet_file = tempfile()
    x = fslbet(infile = filename, 
               outfile = bet_file, 
               opts = bet.opts, 
               betcmd = betcmd, 
               retimg = FALSE)
    bet_file = paste0(tempfile(), ext)
    bet_maskfile = paste0(tempfile(), "_Mask", ext)
    # bet = antsImageRead(bet_file, 3)
    bet_mask = antsImageRead(bet_maskfile, 3)
  }
  
  t1N3 <- antsImageClone(t1)
  
  if (have.other) {
    stopifnot(all(file.exists(other.files)))
    other.imgs = lapply(other.files, antsImageRead, 
                        dimension = 3)
    if (copy_origin) {
      other.imgs = lapply(other.imgs, 
                          antsCopyOrigin,
                          reference = t1N3)
    }
    N3.oimgs = lapply(other.imgs, antsImageClone)
  }
  ## 
  if (correct) {
    if (verbose) {
      message("# Running Bias-Field Correction on file\n")
    }    
    t1N3 = bias_correct(file = t1, 
                        correction = correction, 
                        retimg = TRUE,
                        verbose = verbose)
    t1N3 = oro2ants(t1N3)
    if (have.other) {
      if (verbose) {
        message("# Running Bias-Field Correction on other.files\n")
      }        
      for (i in seq(lother)) {
        N3.oimgs[[i]] = bias_correct(file = other.imgs[[i]], 
                                     correction = correction, 
                                     retimg = TRUE,
                                     verbose = verbose)
        N3.oimgs[[i]] = oro2ants(N3.oimgs[[i]])
      }
    }		
  }
  
  if (skull_strip) {
    t1N3 = maskImage(t1N3, bet_mask)
    if (have.other) {
      N3.oimgs = lapply(N3.oimgs, maskImage,
                        img.mask = bet_mask)
    }
    rm(list = "bet_mask"); gc(); gc();
  }
  
  ## 
  if (is.antsImage(template.file)) {
    template = antsImageClone(template.file)
  } else {
    template.file = checkimg(template.file)
    stopifnot(file.exists(template.file))
    
    template.file = path.expand(template.file)
    template <- antsImageRead(template.file)
  }
  # template.img <- readnii(template.path, reorient = FALSE)
  
  if (verbose) {
    message("# Running Registration of file to template\n")
  }  
  antsRegOut.nonlin <- antsRegistration(
    fixed = template, 
    moving = t1N3, 
    typeofTransform = typeofTransform,  
    outprefix = outprefix,
    verbose = verbose, ...)
  if (verbose) {
    message("# Applying Registration output is\n")
    print(antsRegOut.nonlin)
  }  
  
  if (!all(file.exists(antsRegOut.nonlin$fwdtransforms))) {
    stop("ANTs Registration did not complete, transforms do not exist!")
  }
  if (!all(file.exists(antsRegOut.nonlin$invtransforms))) {
    stop("ANTs Registration did not complete, inverse transforms do not exist!")
  }
  
  if (verbose) {
    message("# Applying Transformations to file\n")
    #     message("# Fixed is \n")
    #     print(template)
    #     message("# Moving is \n")
    #     print(t1N3)
  }  
  t1.to.template <- antsApplyTransforms(
    fixed = template, 
    moving = t1N3,
    transformlist = antsRegOut.nonlin$fwdtransforms,
    interpolator = interpolator) 
  
  
  # moving = t1N3
  transformlist = antsRegOut.nonlin$invtransforms
  # dimension = 3
  
  output = paste0(tempfile(), ".nii.gz")
  
  if (template_to_native) {
    stopifnot(!is.null(invert.file))
    
    if (verbose) {
      message("# Applying Inverse transforms to invert.file\n")
    }  	  
    for (iatlas in seq_along(invert.file)) {
      output = invert.native.fname[iatlas]
      
      
      atlas = antsImageRead(invert.file[iatlas])
      # 			if (!grepl("[.]nii$|[.]nii[.]gz$", output)) {
      # 				output = paste0(output, ".nii.gz")
      # 			}
      
      tmp_img = antsApplyTransforms(fixed = t1N3, 
                                    moving = atlas,
                                    transformlist = transformlist,
                                    interpolator = "NearestNeighbor")
      antsImageWrite(tmp_img, output)
      rm(list = c("tmp_img", "atlas")); gc(); gc();
    }
  }
  
  
  
  if (have.other) {
    if (verbose) {
      message("# Applying Transforms to other.files\n")
    }  	 
    if (is.null(other.init)) {
      reg.oimgs = lapply(N3.oimgs, function(x) {
        antsApplyTransforms(fixed = template, 
                            moving = x, 
                            transformlist = antsRegOut.nonlin$fwdtransforms,
                            interpolator = interpolator
        )
      })
    } else {
      reg.oimgs = mapply(function(x, y) {
        antsApplyTransforms(fixed = template, 
                            moving = x, 
                            transformlist = c(y, antsRegOut.nonlin$fwdtransforms),
                            interpolator = interpolator
        )
      }, N3.oimgs, other.init, SIMPLIFY = FALSE)  
    }
  }
  
  if (verbose) {
    message("# Writing out file\n")
  }  
  antsImageWrite(t1.to.template, outfile)
  rm(list = "t1.to.template"); gc(); gc();
  if (have.other) {
    if (verbose) {
      message("# Writing out other.files\n")
    }      
    for (i in seq(lother)) {
      antsImageWrite(reg.oimgs[[i]], 
                     other.outfiles[i])
    }
    rm(list = c("reg.oimgs", "N3.oimgs")); gc(); gc();
  }
  
  if (remove.warp) {
    if (verbose) {
      message("# Removing Warping images\n")
    }        
    files = unlist(antsRegOut.nonlin[
      c("fwdtransforms", "invtransforms")])
    files = grep("Warp", files, value=TRUE)
    if (length(files) > 0) {
      file.remove(files)
    }
  }
  if (retimg) {
    if (verbose) {
      message("# Reading data back into R\n")
    }          
    img = readnii(outfile, reorient= FALSE)
    outfile = img
  }
  
  L = list(outfile = outfile, 
           fwdtransforms = antsRegOut.nonlin$fwdtransforms,
           invtransforms = antsRegOut.nonlin$invtransforms,
           interpolator = interpolator,
           typeofTransform = typeofTransform,
           retimg = retimg)
  rm(list = c("t1", "t1N3", "template")); gc(); gc();
  rm(list = "antsRegOut.nonlin")
  for (i in 1:10) {
    gc()
  }  
  return(L)
}


