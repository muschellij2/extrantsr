#' @title Registration to a Template
#'
#' @description This function performs registration to a template
#' using ANTsR and SyN transformation, carry out the transformation
#' on other images, and can back-transform atlases
#' in template space to the native space of the iimage
#' @param filename filename of T1 image
#' @param skull_strip do skull stripping with FSL BET 
#' @param correct do Bias correction
#' @param correction N3 or N4 correction, see \code{\link{bias_correct}}
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
#' @param native.cereb Logical indicating if native cerebellum should be 
#' created to \code{native.fname}
#' @param native.fname filename of native cerebellum file
#' @param atlas.file Filename of atlas used for warping labels
#' @param typeofTransform type of transformed used, passed to 
#' \code{\link{antsRegistration}}
#' @param remove.warp (logical) Should warping images be deleted?
#' @param outprefix Character path of where the warp files should be stored.
#' Required if \code{remove.warp = FALSE}
#' @param bet.opts Options passed to \code{\link{fslbet}}
#' @param betcmd BET command used, passed to \code{\link{fslbet}}
#' @param verbose Print diagnostic messages
#' @param ... arguments to \code{\link{antsRegistration}}
#' @import ANTsR
#' @import fslr
#' @import oro.nifti
#' @export
#' @return NULL or object of class nifti for transformed T1 image
ants_regwrite <- function(filename, # filename of T1 image
                   skull_strip = FALSE, # do Skull stripping with FSL BET
	correct = FALSE,  # do N3 Bias correction
	correction = "N3",
	retimg = TRUE, # return a nifti object from function
	outfile = NULL, # output filename, should have .nii or .nii.gz extension
  template.file = file.path(fsldir(), "data", "standard", 
"MNI152_T1_1mm_brain.nii.gz"),
	interpolator="Linear", 
	other.files = NULL,
	other.outfiles= NULL,
	native.cereb = FALSE,
	native.fname = NULL,
	atlas.file = NULL,
	typeofTransform = "SyN",
	remove.warp = TRUE,
  outprefix = NULL,
  bet.opts = "-B -f 0.1 -v",
  betcmd = "bet",
  verbose = TRUE,
	... # arguments to \code{\link{antsRegistration}} 
	){

	if (retimg){
		if (is.null(outfile)) {
	  		outfile = paste0(tempfile(), ".nii.gz")
		}
	} else {
		stopifnot(!is.null(outfile))
	}	
  outfile = path.expand(outfile)

	have.other = FALSE
	if (!is.null(other.files)){
		have.other = TRUE
		lother = length(other.files)
		lout = length(other.outfiles)
		if (lother != lout) {
			stop("Other outfile and infiles must be same length")
		}
	}

	if (native.cereb){
		stopifnot(!is.null(native.fname))
		if (length(atlas.file) != length(native.fname)){
      stop("Length of atlas.files and native.fnames must be equal!")
		}
	}

  if (!remove.warp){
    stopifnot(!is.null(outprefix))
  } else {
    if (is.null(outprefix))  outprefix = tempfile()
  }
  
  if (is(filename, "antsImage")){
    filename = tempants(filename)
  }
  filename = checkimg(filename)
# 	filename = path.expand(filename)
  stopifnot(file.exists(filename))
	t1 <- antsImageRead(filename, 3)

  if (skull_strip){
    if (verbose){
      cat("# Skull Stripping\n")
    }
    ext = get.imgext()
    bet_file = tempfile()
  	x = fslbet(infile = filename, 
           outfile = bet_file, 
           opts = bet.opts, 
           betcmd = betcmd, retimg= FALSE)
    bet_file = paste0(tempfile(), ext)
    bet_maskfile = paste0(tempfile(), "_Mask", ext)
    bet = antsImageRead(bet_file, 3)
    bet_mask = antsImageRead(bet_maskfile, 3)
  }
  
	t1N3 <- antsImageClone(t1)

	if (have.other) {
    other.files = sapply(other.files, checkimg)
	  stopifnot(all(file.exists(other.files)))
		other.imgs = lapply(other.files, antsImageRead, 
			dimension = 3)
		N3.oimgs = lapply(other.imgs, antsImageClone)
	}
	## 
	if (correct){
	  if (verbose){
	    cat("# Running Bias-Field Correction on file\n")
	  }    
    t1N3 = bias_correct(file = t1, 
                        correction = correction, 
                        retimg = TRUE)
    t1N3 = oro2ants(t1N3)
		if (have.other) {
		  if (verbose){
		    cat("# Running Bias-Field Correction on other.files\n")
		  }        
			for (i in seq(lother)){
			  N3.oimgs[[i]] = bias_correct(file = other.imgs[[i]], 
			                      correction = correction, 
			                      retimg = TRUE)
			  N3.oimgs[[i]] = oro2ants(N3.oimgs[[i]])
			}
		}		
	}

	if (skull_strip){
	  t1N3 = maskImage(t1N3, bet_mask)
	  if (have.other) {
  	  N3.oimgs = lapply(N3.oimgs, maskImage,
                        img.mask = bet_mask)
    }
	}
  
	## 
  template.file = checkimg(template.file)
  stopifnot(file.exists(template.file))

  template.file = path.expand(template.file)
	template <- antsImageRead(template.file, 3)
	# template.img <- readNIfTI(template.path, reorient = FALSE)

  if (verbose){
    cat("# Running Registration of file to template\n")
  }  
	antsRegOut.nonlin <- antsRegistration(
		fixed = template, 
		moving = t1N3, 
		typeofTransform = typeofTransform,  
		outprefix = outprefix,
		verbose = verbose, ...)
  if (verbose){
    cat("# Applying Registration output is\n")
    print(antsRegOut.nonlin)
  }  

  if (!all(file.exists(antsRegOut.nonlin$fwdtransforms))){
    stop("ANTs Registration did not complete, transforms do not exist!")
  }
  if (!all(file.exists(antsRegOut.nonlin$invtransforms))){
    stop("ANTs Registration did not complete, inverse transforms do not exist!")
  }

  if (verbose){
    cat("# Applying Transformations to file\n")
#     cat("# Fixed is \n")
#     print(template)
#     cat("# Moving is \n")
#     print(t1N3)
  }  
	t1.to.template <- antsApplyTransforms(fixed=template, 
	  moving=t1N3,
	  transformlist=antsRegOut.nonlin$fwdtransforms,
	  interpolator=interpolator) 


	moving = t1N3
	transformlist = antsRegOut.nonlin$invtransforms
	dimension = 3

	output = paste0(tempfile(), ".nii.gz")

	if (native.cereb){
	  stopifnot(!is.null(atlas.file))
	  
	  if (verbose){
	    cat("# Applying Inverse transforms to atlas.file\n")
	  }  	  
	  for (iatlas in seq_along(atlas.file)){
			output = native.fname[iatlas]

# 			atlas.img <- readNIfTI(atlas.file[iatlas], 
# 				reorient = FALSE)
# 			atlas.img  = cal_img( atlas.img > 0 )
# 
# 			temp.atlas = tempimg(atlas.img)

			atlas = antsImageRead(atlas.file[iatlas])
# 			if (!grepl("[.]nii$|[.]nii[.]gz$", output)){
# 				output = paste0(output, ".nii.gz")
# 			}

			tmp_img = antsApplyTransforms(fixed = t1N3, 
				moving = atlas,
				transformlist = transformlist,
        interpolator = "NearestNeighbor")
      antsImageWrite(tmp_img, output)
		}
	}



	if (have.other) {
	  if (verbose){
	    cat("# Applying Transforms to other.files\n")
	  }  	    
		reg.oimgs = lapply(N3.oimgs, function(x){
			antsApplyTransforms(fixed=template, 
				moving = x, 
	  			transformlist=antsRegOut.nonlin$fwdtransforms,
	  			interpolator=interpolator
	  			)
		})
	}

  if (verbose){
    cat("# Writing out file\n")
  }  
	antsImageWrite(t1.to.template, outfile)

	if (have.other) {
	  if (verbose){
	    cat("# Writing out other.files\n")
	  }      
		for (i in seq(lother)){
			antsImageWrite(reg.oimgs[[i]], 
				other.outfiles[i])
		}
	}

  if (remove.warp){
    if (verbose){
      cat("# Removing Warping images\n")
    }        
    files = unlist(antsRegOut.nonlin[
    	c("fwdtransforms", "invtransforms")])
    files = grep("Warp", files, value=TRUE)
    file.remove(files)
  }
	if (retimg){
	  if (verbose){
	    cat("# Reading data back into R\n")
	  }          
		img = readNIfTI(outfile, reorient= FALSE)
		return(img)
	}
	return(invisible(NULL))
}


