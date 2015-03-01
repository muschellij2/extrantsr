#' @title WhiteStripe Normalization
#'
#' @description This function performs registration to a T1 template
#' using ANTsR and SyN transformation
#' @param t1 filename (or nifti objects) of T1 image
#' @param t2 filename (or nifti objects) of T2 image
#' @param register Register image to template file
#' @param native If images are registered, should the native space
#' normalized image be returned?
#' @param template.file Filename of template to warp to
#' @param typeofTransform type of transformed used, passed to 
#' \code{\link{antsRegistration}} 
#' @param interpolator Interpolation to be performed, passed to
#' \code{\link{antsRegistration}} 
#' @param type Type of whitestripe normalization done
#' @param t1.outfile Output filename of normalized T1 image
#' @param t2.outfile Output filename of normalized T2 image
#' @param other.files Character filenames or list of nifti objects to 
#' normalize.  In the same space as T1.
#' @param other.outfiles Character filenames for output 
#' normalized files. 
#' @param ... arguments to \code{\link{whitestripe}} or 
#' \code{\link{whitestripe_hybrid}}
#' @import WhiteStripe
#' @import fslr
#' @import oro.nifti
#' @import ANTsR
#' @export
#' @return List of nifti objects or character filenames
reg_whitestripe <- function(t1 =NULL, t2 = NULL, 
                            register = TRUE,
                            native = TRUE,
                            template.file = file.path(fsldir(), 
                                                      "data", 
                                                      "standard", 
                                          "MNI152_T1_1mm_brain.nii.gz"),
                            typeofTransform = c("Rigid", "Affine"),
                            interpolator = "LanczosWindowedSinc",
                            type = c("T1", "T2", "hybrid"),
                            t1.outfile = NULL, 
                            t2.outfile = NULL,
                            other.files = NULL,
                            other.outfiles =  NULL,
                            ...
){

  #####################
  # Checking for T1 and T2 iamges
  #####################
  check_outfile = function(outfile, retimg){
    if (retimg){
      if (is.null(outfile)) {
        outfile = paste0(tempfile(), ".nii.gz")
      } 
    } else {
      stopifnot(!is.null(outfile))
    }	
    return(path.expand(outfile))
  }
  
  typeofTransform = match.arg(typeofTransform, c("Rigid", "Affine"))
  type = match.arg(type, c("T1", "T2", "hybrid"))
  #####################
  # Checking for T1 and T2 iamges
  #####################
  nullt1 = is.null(t1)
  nullt2 = is.null(t2)
  if (nullt1 & nullt2){
    stop("Need a T1 or T2 image")
  }
  
  #####################
  # Creating output files
  #####################
  if (!nullt1){
    # reading in T1
    t1 = checkimg(t1)    
    if (is.null(t1.outfile)){
      stop("T1 outfile needs te specified if T1 specified")
    }
    t1.outfile = path.expand(t1.outfile)
    ##################
    # Must have extension
    ##################
    if (!all(grepl("[.]nii", c(t1.outfile)))){
      stop("t1 outfile must be nifti .nii or .nii.gz")
    }     
  } 
  
  if (!nullt2){
    # reading in T2
    t2 = checkimg(t2) 
    if (is.null(t2.outfile)){
      stop("T2 outfile needs te specified if T2 specified")
    }
    t2.outfile = path.expand(t2.outfile)
    ##################
    # Must have extension
    ##################
    if (!all(grepl("[.]nii", c(t2.outfile)))){
      stop("t2 outfile must be nifti .nii or .nii.gz")
    }     
  }
  nullother = is.null(other.files)
  
  if (!nullother){
    stopifnot(length(other.files)== length(other.outfiles))
    ##################
    # Must have extension
    ##################
    if (!all(grepl("[.]nii", c(other.outfiles)))){
      stop("All filenames must be nifti .nii or .nii.gz")
    }    
    other.files = sapply(other.files, checkimg)
  }
  
  ##### everything filesnames from here
  
  ###################
  # Perform Registration
  ###################    
  if (register){
    if (!nullother){
      other.temp = sapply(seq(other.files), 
                          function(x) tempfile(fileext = '.nii.gz'))
    } else {
      other.temp = NULL
    }
    outprefix = tempfile()
    if (!nullt1){
      ###################
      # Carry T2 with transformation
      ###################    
      if (!nullt2){
        other.temp = c(t2 = tempfile(fileext = '.nii.gz'), 
                     other.temp)
      }
      ###################
      # Register
      ###################  
      outfile = tempfile(fileext = '.nii.gz')
      ants_regwrite(filename = t1, 
                    retimg = FALSE,
                    outfile = outfile,
                    template.file = template.file,
                    typeofTransform = typeofTransform,
                    interpolator = interpolator,
                    outprefix = outprefix,
                    remove.warp = TRUE,
                    other.files = other.files,
                    other.outfiles = other.temp)
      t1 = check_nifti(outfile)
      if (!nullt2){
        t2 = other.temp[1]
        t2 = check_nifti(t2)
        other.temp = other.temp[-1]
      }      
      if (!nullother){
        other.files = lapply(other.temp, check_nifti)
      }
    } else {
      stop("Registration must be done with the T1 image")
    }
  }
  
  if (!nullt1){
    t1 = check_nifti(t1)
  }
  if (!nullt2){
    t2 = check_nifti(t2)
  }
  if (!nullother){
    other.files = lapply(other.files, check_nifti)
  }
  
  ########### everything is images from here on out
  
  ###################
  # Different Scenarios
  ###################  
  if (type == "T1"){
    if (!nullt2){
      stop(paste0("T2 should not be specified when type = ", 
                  "'T1', put in other.files"))
    }
    ws = whitestripe( t1, type = "T1", ...)
  }
  if (type == "T2"){
    if (!nullt1){
      stop(paste0("T1 should not be specified when type = ", 
          "'T2', put in other.files"))
    }    
    ws = whitestripe( t2, type = "T2", ...)
  }  
  if (type == "hybrid"){
    ws = whitestripe_hybrid( t1 = t1, t2 = t2, ...)
  }
  
  ##########################
  # Get whitestripe indices
  ##########################
  indices = ws$whitestripe.ind
  
  dtype = function(img){
    img = drop_img_dim(img)
    img = datatype(img, 
                        datatype= convert.datatype()$FLOAT32,
                        bitpix= convert.bitpix()$FLOAT32)
    return(img)
  }
  ##########################
  # Apply WhiteStripe
  ##########################  
  if (!nullt1){
    t1 = dtype(whitestripe_norm(t1, indices = indices))
  }
  if (!nullt2){
    t2 = dtype(whitestripe_norm(t2, indices = indices))
  }
  if (!nullother){
    other.files = lapply(other.files, function(x){
      dtype(whitestripe_norm(x, indices = indices))
    })
  }

  ###################
  # Perform Registration
  ###################    
  if (native){
#     if (!register){
#       warning(paste0("Native is TRUE, but register is FALSE,",
#         "returning out images"))
#     }
    if (register){
      inv.trans = paste0(outprefix, "0GenericAffine.mat")
      template.img = antsImageRead(template.file, dimension = 3)
      if (!nullt1){
        ##############################
        # Applying Transformation
        ##############################
        fixed = oro2ants(t1)
        fixed = antsApplyTransforms(fixed = fixed, 
                                   moving = template.img, 
                                   transformlist = inv.trans, 
                                   interpolator = interpolator, 
                                   whichtoinvert = 1)
        t1 = ants2oro(fixed)
        ###################
        # Carry T2 with transformation
        ###################    
        if (!nullt2){
          fixed = oro2ants(t2)
          fixed = antsApplyTransforms(fixed = fixed, 
                                      moving = template.img, 
                                      transformlist = inv.trans, 
                                      interpolator = interpolator, 
                                      whichtoinvert = 1)
          t2 = ants2oro(fixed)
        }
        ###################
        # Register
        ###################  
        if (!nullother){
          for (ifile in seq_along(other.files)){
            fixed = oro2ants(other.files[[ifile]])
            fixed = antsApplyTransforms(fixed = fixed, 
                                        moving = template.img, 
                                        transformlist = inv.trans, 
                                        interpolator = interpolator, 
                                        whichtoinvert = 1)
            other.files[[ifile]] = ants2oro(fixed)
          }
        }
      } else {
        stop("Registration must be done with the T1 image")
      }
    }
  }
  
  ###################
  # Write out images
  ###################      
  if (!nullt1){
    writeNIfTI(t1, filename = t1.outfile)
  }
  if (!nullt2){
    writeNIfTI(t2, filename = t2.outfile)
  }
  if (!nullother){
    mapply(function(img, fname){
      writeNIfTI(img, filename = fname)
    }, other.files, other.outfiles)
  }
  
  return(list(t1 = t1, t2 = t2, other.files = other.files))
}



