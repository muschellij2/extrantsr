#' @title Flip Image with Registration
#'
#' @description This function performs registration to a template
#' using ANTsR and will run \code{\link{fslswapdim}}
#' @param t1 filename (or nifti objects) of T1 image
#' @param register Register image to template file
#' @param native If images are registered, should the native space
#' normalized image be returned?
#' @param template.file Filename of template to warp to
#' @param typeofTransform type of transformed used, passed to 
#' \code{\link{antsRegistration}} 
#' @param interpolator Interpolation to be performed, passed to
#' \code{\link{antsRegistration}} 
#' @param t1.outfile Output filename of normalized T1 image
#' @param other.files Character filenames or list of nifti objects to 
#' normalize.  In the same space as T1.
#' @param other.outfiles Character filenames for output 
#' normalized files. 
#' @param mask File or nifti image of brain mask  
#' @param mask.outfile Character filename for output 
#' brain mask.  
#' @param verbose Print Diagnostic Messages
#' @param a Option for x domain in \code{\link{fslswapdim}}
#' @param b Option for y domain in \code{\link{fslswapdim}}
#' @param c Option for z domain in \code{\link{fslswapdim}}
#' @param reproducible Sets the seed and 
#' \code{Sys.setenv(ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS = 1)}.
#'  See
#' \url{https://github.com/ANTsX/ANTs/wiki/antsRegistration-reproducibility-issues}
#' for discussion.
#' @param seed will execute 
#' \code{Sys.setenv(ANTS_RANDOM_SEED = seed)} before
#' running to attempt a more reproducible result.   If \code{NULL}, will not set anything, 
#' but \code{reproducible} must be \code{FALSE}.   
#' @param ... arguments to \code{\link{fslswapdim}}
#' @export
#' @importFrom fslr fsldir fslswapdim fslmask
#' @return List of nifti objects or character filenames
reg_flip <- function(t1, 
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
                     t1.outfile = NULL, 
                     other.files = NULL,
                     other.outfiles =  NULL,
                     mask = NULL,
                     mask.outfile = NULL,
                     verbose = TRUE,
                     a = "-x", b = "y", c = "z",
                     reproducible = TRUE,
                     seed = 1,                     
                     ...
){
  
  #####################
  # Checking for T1 and T2 iamges
  #####################
  typeofTransform = match.arg(typeofTransform, c("Rigid", "Affine"))
  #####################
  # Checking for T1 and T2 iamges
  #####################
  nullt1 = is.null(t1)
  if (nullt1){
    stop("Need a T1 image")
  }
  
  ####################
  # Mask file
  ####################  
  
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
    maskants = antsImageRead(filename = mask, dimension = 3)
    # maskants = antsImageClone(mm)
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
      fslmask(file = t1, mask = mask, outfile = tfile, verbose = verbose)
      ext = get.imgext()
      t1 = paste0(tfile, ext)
      rm(list="tfile")
    }
    # reading in T1
    t1 = checkimg(t1)
    if (is.null(t1.outfile)){
      t1.outfile = tempfile(fileext = ".nii.gz")
    }
    t1.outfile = path.expand(t1.outfile)
    t1ants = antsImageRead(filename = t1, dimension = 3)
    ##################
    # Must have extension
    ##################
    if (!all(grepl("[.]nii", c(t1.outfile)))){
      stop("t1 outfile must be nifti .nii or .nii.gz")
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
      antsImageRead(filename = x, dimension = 3)
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
                    verbose = verbose,
                    reproducible = reproducible,
                    seed = seed)
      t1 = check_nifti(outfile)
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
  if (!nullmask){
    mask = cal_img(check_nifti(mask) > 0.5)
  }
  if (!nullother){
    other.files = lapply(other.files, check_nifti)
  }
  
  ########### everything is images from here on out
  
  dtype = function(img){
    img = drop_img_dim(img)
    img = datatyper(img, 
                    datatype= convert.datatype()$FLOAT32,
                    bitpix= convert.bitpix()$FLOAT32)
    return(img)
  }
  ##########################
  # Apply Z-score
  ##########################  
  if (verbose){
    message("# Running Flipping\n")
  }
  if (!nullt1){
    t1 = dtype(fslswapdim(t1, retimg = TRUE, 
                          a = a, 
                          b = b, 
                          c = c, 
                          verbose = verbose, ...))
  }
  if (!nullother){
    other.files = lapply(other.files, function(x){
      dtype(fslswapdim(x, retimg = TRUE, 
                       a = a, 
                       b = b, 
                       c = c, 
                       verbose = verbose,                        
                       ...)
            
      )    
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
      # template.img = antsImageRead(template.file, dimension = 3)
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
        rm(list = "fixed"); gc(); gc();
        ###################
        # Use Native mask Image
        ###################           
        if (!nullmask){
          mask = ants2oro(maskants)
          rm(list = "maskants"); gc(); gc();
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
            rm(list = "fixed"); gc(); gc();
          }
          rm(list = "other.ants"); gc(); gc();
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
    message("# Writing out Flipped Images\n")
  }  
  if (!nullt1){
    if (verbose){
      message("# Writing out Flipped T1\n")
    }      
    t1 = cal_img(t1)
    if (!nullmask){
      t1 = mask_img(t1, mask)
    }
    writeNIfTI(t1, filename = nii.stub(t1.outfile))
  }
  if (!nullother){
    if (verbose){
      message("# Writing out Flipped Other Files\n")
    }      
    print(other.outfiles)
    mapply(function(img, fname){
      img = cal_img(img)
      if (!nullmask){
        img = mask_img(img, mask)
      }
      writeNIfTI(img, filename = nii.stub(fname))
    }, other.files, other.outfiles)
    rm(list = c("other.files", "other.outfiles"))
    for (i in 1:10) {
      gc()
    }
  }
  if (!nullmask.outfile){
    if (verbose){
      message("# Writing out Brain Mask Outfile\n")
    }    
    mask = cal_img(mask)
    writeNIfTI(mask, filename = nii.stub(mask.outfile))
    rm(list = c("other.files", "other.outfiles"))
    for (i in 1:10) {
      gc()
    }    
  }  
  
  return(list(t1 = t1, 
              other.files = other.files, 
              native = native,
              register = register
              #               template.file = template.file
  ))
}



