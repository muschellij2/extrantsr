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
#' @param ws.outfile Character filename for output 
#' whitestripe mask.  
#' @param mask File or nifti image of brain mask  
#' @param mask.outfile Character filename for output 
#' brain mask.  
#' @param verbose Print Diagnostic Messages
#' @param reproducible Sets the seed and 
#' \code{Sys.setenv(ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS = 1)}.
#'  See
#' \url{https://github.com/ANTsX/ANTs/wiki/antsRegistration-reproducibility-issues}
#' for discussion.
#' @param seed will execute 
#' \code{Sys.setenv(ANTS_RANDOM_SEED = seed)} before
#' running to attempt a more reproducible result.   If \code{NULL}, will not set anything, 
#' but \code{reproducible} must be \code{FALSE}.     
#' @param ... arguments to \code{\link{whitestripe}} or 
#' \code{\link{whitestripe_hybrid}}
#' @export
#' @return List of nifti objects or character filenames
#' @importFrom fslr fsldir fslmask
#' @importFrom WhiteStripe whitestripe whitestripe_hybrid whitestripe_norm
#' @importFrom ANTsRCore antsImageRead antsImageClone
#' @importFrom oro.nifti cal_img writeNIfTI
reg_whitestripe <- function(
  t1 =NULL, t2 = NULL, 
  register = TRUE,
  native = TRUE,
  template.file = file.path(fsldir(), 
                            "data", 
                            "standard", 
                            paste0("MNI152_T1_1mm", 
                                   ifelse(is.null(mask), "", 
                                          "_brain"), 
                                   ".nii.gz")),
  typeofTransform = c("Rigid", "Affine"),
  interpolator = "LanczosWindowedSinc",
  type = c("T1", "T2", "hybrid"),
  t1.outfile = NULL, 
  t2.outfile = NULL,
  other.files = NULL,
  other.outfiles =  NULL,
  ws.outfile = NULL,
  mask = NULL,
  mask.outfile = NULL,
  verbose = TRUE,
  reproducible = TRUE,
  seed = 1,      
  ...
){
  
  #####################
  # Checking for T1 and T2 iamges
  #####################
  
  
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
  
  ####################
  # WhiteStripe file
  ####################  
  nullws = is.null(ws.outfile)
  if (nullws){
    ws.outfile = tempfile(fileext = ".nii.gz")
  }
  
  nullmask = is.null(mask)
  nullmask.outfile = is.null(mask.outfile)
  
  #####################
  # Creating output mask
  #####################
  if (!nullmask){
    if (verbose){
      message("# Doing Checks on Mask \n")
    }    
    # reading in T1
    mask = checkimg(mask)
    if (nullmask.outfile){
      mask.outfile = tempfile(fileext = ".nii.gz")
    }
    mm = ANTsRCore::antsImageRead(filename = mask, dimension = 3)
    maskants = ANTsRCore::antsImageClone(mm)
    ##################
    # Must have extension
    ##################
    #     if (!all(grepl("[.]nii", c(mask.outfile)))){
    #       stop("t1 outfile must be nifti .nii or .nii.gz")
    #     }     
  } 
  
  #####################
  # Creating output files
  #####################
  if (!nullt1){
    #############
    # Mask the images
    ###########
    if (verbose){
      message("# Doing Checks on T1 \n")
    }        
    if (!nullmask){
      tfile = tempfile()
      fslmask(file = t1, mask = mask, 
              outfile = tfile, verbose = verbose)
      ext = get.imgext()
      t1 = paste0(tfile, ext)
      rm(list="tfile")
    }
    # reading in T1
    t1 = checkimg(t1)
    if (is.null(t1.outfile)){
      if (type %in% c("T1", "hybrid")){
        stop("T1 outfile needs te specified if T1 specified")
      } else {
        t1.outfile = tempfile(fileext = ".nii.gz")
      }
    }
    t1.outfile = path.expand(t1.outfile)
    t1ants = ANTsRCore::antsImageRead(filename = t1, dimension = 3)
    ##################
    # Must have extension
    ##################
    if (!all(grepl("[.]nii", c(t1.outfile)))){
      stop("t1 outfile must be nifti .nii or .nii.gz")
    }     
  } 
  
  if (!nullt2){
    #############
    # Mask the images
    ###########
    if (verbose){
      message("# Doing Checks on T2 \n")
    }         
    if (!nullmask){
      tfile = tempfile()
      fslmask(file = t2, mask = mask, 
              outfile = tfile, verbose = verbose)
      ext = get.imgext()
      t2 = paste0(tfile, ext)
      rm(list="tfile")      
    }
    # reading in T2
    t2 = checkimg(t2) 
    if (is.null(t2.outfile)){
      stop("T2 outfile needs te specified if T2 specified")
    }
    t2.outfile = path.expand(t2.outfile)
    t2ants = ANTsRCore::antsImageRead(filename = t2, dimension = 3)    
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
    #############
    # Mask the images
    ###########
    if (verbose){
      message("# Doing Checks on Other files \n")
    }     
    if (!nullmask){
      other.files = sapply(other.files, function(fname){
        tfile = tempfile()
        fslmask(file = fname, mask = mask, outfile = tfile, verbose = verbose)
        ext = get.imgext()
        tfile = paste0(tfile, ext)
        return(tfile)
      })      
    }    
    other.files = sapply(other.files, checkimg)    
    other.ants = lapply(other.files, function(x){
      ANTsRCore::antsImageRead(filename = x, dimension = 3)
    }) 
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
        other.files = c(t2 = t2, 
                        other.files)
        other.temp = c(t2 = tempfile(fileext = '.nii.gz'), 
                       other.temp)
      }
      
      ###################
      # Carry Mask with transformation
      ###################    
      if (!nullmask){
        other.files = c(other.files, mask = mask)
        other.temp = c(other.temp, mask = mask.outfile)
      }
      
      ###################
      # Register
      ###################  
      if (verbose){
        message("# Registering to Template \n")
      }        
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
                    other.outfiles = other.temp,
                    verbose = verbose)
      t1 = check_nifti(outfile)
      if (!nullt2){
        t2 = other.temp[1]
        t2 = check_nifti(t2)
        other.temp = other.temp[-1]
        other.files = other.files[-1]
      }  
      if (!nullmask){
        mask = other.temp[length(other.temp)]
        mask = cal_img(check_nifti(mask) > 0.5)
        other.temp = other.temp[-length(other.temp)]
        other.files = other.files[-length(other.temp)]
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
  if (!nullmask){
    mask = cal_img(check_nifti(mask) > 0.5)
  }
  if (!nullother){
    other.files = lapply(other.files, check_nifti)
  }
  
  ########### everything is images from here on out
  
  ###################
  # Different Scenarios
  ###################  
  if (verbose){
    message("# Running WhiteStripe Estimation \n")
  }     
  if (type == "T1"){
    if (!nullt2){
      stop(paste0("T2 should not be specified when type = ", 
                  "'T1', put in other.files"))
    }
    ws = whitestripe( t1, type = "T1", ...)
  }
  if (type == "T2"){
    if (!nullt1){
      if (!register){
        stop(paste0("T1 should not be specified when type = ", 
                    "'T2', put in other.files"))
      }
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
  mask.img = ws$mask.img
  
  dtype = function(img){
    img = drop_img_dim(img)
    img = datatyper(img, 
                    datatype= convert.datatype()$FLOAT32,
                    bitpix= convert.bitpix()$FLOAT32)
    return(img)
  }
  ##########################
  # Apply WhiteStripe
  ##########################  
  if (verbose){
    message("# Running WhiteStripe Normalization\n")
  }
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
    if (verbose){
      message("# Returning images to Native Space\n")
    }    
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
        fixed = antsApplyTransforms(fixed = t1ants, 
                                    moving = fixed, 
                                    transformlist = inv.trans, 
                                    interpolator = interpolator, 
                                    whichtoinvert = 1)
        t1 = ants2oro(fixed)
        if (!nullws){
          fixed = oro2ants(mask.img)
          fixed = antsApplyTransforms(fixed = t1ants, 
                                      moving = fixed, 
                                      transformlist = inv.trans, 
                                      interpolator = interpolator, 
                                      whichtoinvert = 1)
          mask.img = cal_img(ants2oro(fixed) > 0.5)
        }
        ###################
        # Carry T2 with transformation
        ###################    
        if (!nullt2){
          fixed = oro2ants(t2)
          fixed = antsApplyTransforms(fixed = t2ants, 
                                      moving = fixed, 
                                      transformlist = inv.trans, 
                                      interpolator = interpolator, 
                                      whichtoinvert = 1)
          t2 = ants2oro(fixed)
        }
        ###################
        # Use Native mask Image
        ###################           
        if (!nullmask){
          mask = ants2oro(maskants)
        }    
        ###################
        # Register
        ###################  
        if (!nullother){
          for (ifile in seq_along(other.files)){
            fixed = oro2ants(other.files[[ifile]])
            aimg = other.ants[[ifile]]
            fixed = antsApplyTransforms(fixed = aimg, 
                                        moving = fixed, 
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
  if (verbose){
    message("# Writing out Normalized Images\n")
  }  
  if (!nullt1){
    if (verbose){
      message("# Writing out Normalized T1\n")
    }      
    t1 = cal_img(t1)
    t1 = mask_img(t1, mask)
    writeNIfTI(t1, filename = nii.stub(t1.outfile))
  }
  if (!nullt2){
    if (verbose){
      message("# Writing out Normalized T2\n")
    }      
    t2 = cal_img(t2)
    t2 = mask_img(t2, mask)
    writeNIfTI(t2, filename = nii.stub(t2.outfile))
  }
  if (!nullother){
    if (verbose){
      message("# Writing out Normalized Other Files\n")
    }      
    print(other.outfiles)
    mapply(function(img, fname){
      img = cal_img(img)
      img = mask_img(img, mask)
      writeNIfTI(img, filename = nii.stub(fname))
    }, other.files, other.outfiles)
  }
  if (!nullws){
    if (verbose){
      message("# Writing out WhiteStripe Mask\n")
    }          
    mask.img = cal_img(mask.img)
    writeNIfTI(mask.img, filename = nii.stub(ws.outfile))
  } else {
    mask.img = NULL
  }
  if (!nullmask.outfile){
    if (verbose){
      message("# Writing out Brain Mask Outfile\n")
    }    
    mask = cal_img(mask)
    writeNIfTI(mask, filename = nii.stub(mask.outfile))
  }  
  
  return(list(t1 = t1, 
              t2 = t2, 
              other.files = other.files, 
              mask.img = mask.img,
              native = native,
              register = register,
              ws.type = type
              #               template.file = template.file
  ))
}



