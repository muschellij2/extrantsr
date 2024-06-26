#' @title Registration Wrapper function
#'
#' @description This function performs registration
#' using ANTsR, carries out the transformation
#' on other images, and can back-transform image
#' in registered space to the native space of the original image.  Returns the
#' transforms
#'
#' @param filename filename of image to be registered
#' @param skull_strip do skull stripping with FSL BET
#' @param correct do bias correction on the filename
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
#' @param other_interpolator interpolation done for
#' \code{\link{antsApplyTransforms}} for \code{other.files}
#' @param other.init Initial transformation lists (same length as \code{other.files})
#' to use in before the estimated transformation for one interpolation
#' @param invert.native.fname filename of output native file, must have
#' same length as \code{invert.file} or be \code{NULL}.
#' @param invert.file Filename of image to invert to native space
#' @param invert_interpolator interpolation done for
#' \code{\link{antsApplyTransforms}} for \code{invert.file} 
#' @param typeofTransform type of transformed used, passed to
#' \code{\link{antsRegistration}}
#' @param remove.warp (logical) Should warping images be deleted?
#' @param outprefix Character path of where the warp files should be stored.
#' Required if \code{remove.warp = FALSE}
#' @param bet.opts Options passed to \code{\link{fslbet}}
#' @param betcmd BET command used, passed to \code{\link{fslbet}}
#' @param copy_origin Copy image origin from t1, using \code{\link{antsCopyOrigin}}
#' @param verbose Print diagnostic messages
#' @param force_registration If \code{TRUE}, then registration will 
#' be run even if transforms exist.
#' @param reproducible Sets the seed and 
#' \code{Sys.setenv(ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS = 1)}.
#'  See
#' \url{https://github.com/ANTsX/ANTs/wiki/antsRegistration-reproducibility-issues}
#' for discussion.
#' @param seed will execute 
#' \code{Sys.setenv(ANTS_RANDOM_SEED = seed)} before
#' running to attempt a more reproducible result.   If \code{NULL}, will not set anything, 
#' but \code{reproducible} must be \code{FALSE}.  
#' @param ... arguments to \code{\link{antsRegistration}}
#'
#' @export
#' @return List of the output filenames and transformations
registration <- function(
  filename,
  skull_strip = FALSE,
  correct = FALSE,
  correction = "N4",
  retimg = TRUE,
  outfile = NULL,
  template.file = file.path(fsldir(), "data",
                            "standard",
                            "MNI152_T1_1mm_brain.nii.gz"),
  interpolator = "Linear",
  other_interpolator = interpolator,
  other.files = NULL,
  other.outfiles = NULL,
  other.init = NULL,
  invert_interpolator = interpolator,
  invert.native.fname = NULL,
  invert.file = NULL,
  typeofTransform = "SyN",
  remove.warp = FALSE,
  outprefix = NULL,
  bet.opts = "-B -f 0.1 -v",
  betcmd = "bet",
  copy_origin = TRUE,
  verbose = TRUE,
  reproducible = TRUE,
  seed = 1,
  force_registration = TRUE,
  ...) {
  
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
  outfile = check_outfile(outfile = outfile, retimg = retimg)
  
  have.other = FALSE
  if (!is.null(other.files)) {
    have.other = TRUE
    other.files = checkimg(other.files)
    if (!is.null(other.outfiles)) {
      other.outfiles = checkimg(other.outfiles)
    } else {
      other.outfiles = sapply(other.outfiles, function(x) {
        tempfile(fileext = ".nii.gz")
      })
    }
    
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
  
  if (!is.null(invert.file)) {
    invert.file = checkimg(invert.file)
    if (is.null(invert.native.fname)) {
      invert.native.fname = sapply(seq_along(invert.file), function(x) {
        tempfile(fileext = ".nii.gz")
      })
    }
    
    if (length(invert.file) != length(invert.native.fname)) {
      stop("Length of invert.file and invert.native.fnames must be equal!")
    }
  }
  
  # if (!remove.warp) {
  # stopifnot(!is.null(outprefix))
  # } else {
  if (is.null(outprefix)) {
    outprefix = tempfile()
  }
  dir.create(dirname(outprefix), showWarnings = FALSE,
             recursive = TRUE)
  # }
  
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
    x = fslbet(
      infile = filename,
      outfile = bet_file,
      opts = bet.opts,
      betcmd = betcmd,
      retimg = FALSE
    )
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
    t1N3 = bias_correct(
      file = t1,
      correction = correction,
      retimg = TRUE,
      verbose = verbose
    )
    t1N3 = oro2ants(t1N3)
    if (have.other) {
      if (verbose) {
        message("# Running Bias-Field Correction on other.files\n")
      }
      for (i in seq(lother)) {
        N3.oimgs[[i]] = bias_correct(
          file = other.imgs[[i]],
          correction = correction,
          retimg = TRUE,
          verbose = verbose
        )
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
    rm(list = "bet_mask")
    gc()
    
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
  out_trans = c(Affine = "0GenericAffine.mat", 
                fwd = "1Warp.nii.gz", inv = "1InverseWarp.nii.gz")
  n_trans = names(out_trans)
  out_trans = paste0(
    outprefix, out_trans)
  names(out_trans) = n_trans
  
  if ( !all(file.exists(out_trans)) || force_registration) {
    antsRegOut.nonlin <- antsRegistration(
      fixed = template,
      moving = t1N3,
      typeofTransform = typeofTransform,
      outprefix = outprefix,
      verbose = verbose,
      ...)
  } else {
    antsRegOut.nonlin = list(
      fwdtransforms = unname(out_trans[c("fwd", "Affine")]),
      invtransforms = unname(out_trans[c("Affine", "inv")])
    )
  }
  ######################################################
  # added this to try to wrap up the gc()
  antsRegOut.nonlin$warpedmovout = NULL
  antsRegOut.nonlin$warpedfixout = NULL
  ######################################################
  for (i in 1:5) {
    gc()
  }
  # fixing multi-naming convention problem
  fwd = antsRegOut.nonlin$fwdtransforms
  fwd = fwd[grepl("Generic|Warp", fwd)]
  antsRegOut.nonlin$fwdtransforms = fwd
  
  inv = antsRegOut.nonlin$invtransforms
  inv = inv[grepl("Generic|Warp", inv)]  
  antsRegOut.nonlin$invtransforms = inv
  
  
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
    interpolator = interpolator,
    verbose = verbose
  )
  
  
  # moving = t1N3
  transformlist = antsRegOut.nonlin$invtransforms
  # dimension = 3
  
  output = paste0(tempfile(), ".nii.gz")
  
  if (!is.null(invert.file)) {
    
    if (verbose) {
      message("# Applying Inverse transforms to invert.file\n")
    }
    for (iatlas in seq_along(invert.file)) {
      output = invert.native.fname[iatlas]
      
      atlas = antsImageRead(invert.file[iatlas])
      # 			if (!grepl("[.]nii$|[.]nii[.]gz$", output)) {
      # 				output = paste0(output, ".nii.gz")
      # 			}
      
      tmp_img = antsApplyTransforms(
        fixed = t1N3,
        moving = atlas,
        transformlist = transformlist,
        interpolator = invert_interpolator,
        verbose = verbose
      )
      antsImageWrite(tmp_img, output)
      rm(list = c("tmp_img", "atlas"))
      gc()

    }
  }
  
  
  
  if (have.other) {
    if (verbose) {
      message("# Applying Transforms to other.files\n")
    }
    if (is.null(other.init)) {
      reg.oimgs = lapply(N3.oimgs, function(x) {
        antsApplyTransforms(
          fixed = template,
          moving = x,
          transformlist = antsRegOut.nonlin$fwdtransforms,
          interpolator = other_interpolator,
          verbose = verbose
        )
      })
    } else {
      reg.oimgs = mapply(function(x, y) {
        antsApplyTransforms(
          fixed = template,
          moving = x,
          transformlist = c(antsRegOut.nonlin$fwdtransforms, y),
          interpolator = other_interpolator,
          verbose = verbose
        )
      }, N3.oimgs, other.init, SIMPLIFY = FALSE)
    }
  }
  
  if (verbose) {
    message("# Writing out file\n")
  }
  antsImageWrite(t1.to.template, outfile)
  if (verbose) {
    print(outfile)
  }  
  rm(list = "t1.to.template")
  gc()

  if (have.other) {
    if (verbose) {
      message("# Writing out other.files\n")
    }
    for (i in seq(lother)) {
      antsImageWrite(reg.oimgs[[i]],
                     other.outfiles[i])
      if (verbose) {
        print(other.outfiles[i])
      } 
    }
    rm(list = c("reg.oimgs", "N3.oimgs"))
    gc()

  }
  
  if (remove.warp) {
    if (verbose) {
      message("# Removing Warping images\n")
    }
    files = unlist(antsRegOut.nonlin[c("fwdtransforms", "invtransforms")])
    files = grep("Warp", files, value = TRUE)
    if (length(files) > 0) {
      file.remove(files)
    }
  }
  if (retimg) {
    if (verbose) {
      message("# Reading data back into R\n")
    }
    outfile = readnii(outfile, reorient = FALSE)
  }
  
  L = list(
    outfile = outfile,
    fwdtransforms = antsRegOut.nonlin$fwdtransforms,
    invtransforms = antsRegOut.nonlin$invtransforms,
    interpolator = interpolator,
    other_interpolator = other_interpolator,
    invert_interpolator = invert_interpolator,
    typeofTransform = typeofTransform,
    retimg = retimg
  )
  L$inverted.outfiles = invert.native.fname
  
  L$other.outfiles = other.outfiles
  rm(list = c("t1", "t1N3", "template"))
  gc()
  
  rm(list = "antsRegOut.nonlin")
  for (i in 1:5) {
    gc()
  }
  return(L)
}
