#' @title Registration to T1 Template
#'
#' @description This function performs registration to a T1 template
#' using ANTsR and SyN transformation
#' @param filename filename of T1 image
#' @param skull_strip do skull stripping with FSL BET 
#' @param n3correct do N3 Bias correction
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
#' @param ... arguments to \code{\link{antsApplyTransforms}}
#' @import ANTsR
#' @import fslr
#' @import oro.nifti
#' @export
#' @return NULL or object of class nifti for transformed T1 image
ants_regwrite <- function(filename, # filename of T1 image
                   skull_strip = FALSE, # do Skull stripping with FSL BET
	n3correct = FALSE,  # do N3 Bias correction
	retimg = TRUE, # return a nifti object from function
	outfile = NULL, # output filename, should have .nii or .nii.gz extension
  template.file = file.path(fsldir(), "data", "standard", 
"MNI152_T1_1mm_brain.nii.gz"),
	interpolator="Linear", # interpolation done for \code{\link{antsApplyTransforms}}
	other.files = NULL,
	other.outfiles= NULL,
	native.cereb = FALSE,
	native.fname = NULL,
	atlas.file = NULL,
	typeofTransform = "SyN",
	remove.warp = FALSE,
  outprefix = NULL,
  bet.opts = "-B -f 0.1 -v",
  betcmd = "bet",
	... # arguments to \code{\link{antsApplyTransforms}} 
	){

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
  outfile = path.expand(outfile)

	have.other = FALSE
	if (!is.null(other.files)){
		have.other = TRUE
		lother = length(other.files)
		lout = length(other.outfiles)
		if (lother != lout) {
			stopifnot("Other outfile and infiles must be same length")
		}
	}

	if (native.cereb){
		stopifnot(!is.null(native.fname))
	}

  if (!remove.warp){
    stopifnot(!is.null(outprefix))
  } else {
    if (is.null(outprefix))  outprefix = tempfile()
  }
  
	filename = path.expand(filename)
	t1 <- antsImageRead(filename, 3)

  if (skull_strip){
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
		other.imgs = lapply(other.files, antsImageRead, 
			dimension = 3)
		N3.oimgs = lapply(other.imgs, antsImageClone)
	}
	## 
	if (n3correct){
		N3BiasFieldCorrection(t1@dimension, t1, t1N3, "4")
		if (have.other) {
			for (i in seq(lother)){
				N3BiasFieldCorrection(other.imgs[[i]]@dimension, 
					other.imgs[[i]], N3.oimgs[[i]], "4")
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
  template.file = path.expand(template.file)
	template <- antsImageRead(template.file, 3)
	# template.img <- readNIfTI(template.path, reorient = FALSE)


	antsRegOut.nonlin <- antsRegistration(
		fixed = template, 
		moving = t1N3, 
		typeofTransform = typeofTransform,  
		outprefix = outprefix)


	t1.to.template <- antsApplyTransforms(fixed=template, 
	  moving=t1N3 ,
	  transformlist=antsRegOut.nonlin$fwdtransforms,
	  interpolator=interpolator) 


	moving = t1N3
	transformlist = antsRegOut.nonlin$invtransforms
	dimension = 3

	output = paste0(tempfile(), ".nii.gz")

	if (native.cereb){
	  stopifnot(!is.null(atlas.file))
	  
	  for (iatlas in seq_along(atlas.file)){
			output = native.fname[iatlas]

			atlas.img <- readNIfTI(atlas.file[iatlas], 
				reorient = FALSE)
			atlas.img  = cal_img( atlas.img > 0 )

			temp.atlas = tempimg(atlas.img)

			fixed = temp.atlas
			if (!grepl("[.]nii$|[.]nii[.]gz$", output)){
				output = paste0(output, ".nii.gz")
			}
			invwarp(3, fixed = fixed, 
				output = output, 
				moving = moving,
				transformlist = transformlist)
		}
	}



	if (have.other) {
		reg.oimgs = lapply(N3.oimgs, function(x){
			antsApplyTransforms(fixed=template, 
				moving = x, 
	  			transformlist=antsRegOut.nonlin$fwdtransforms,
	  			interpolator=interpolator
	  			)
		})
	}

	antsImageWrite(t1.to.template, outfile)

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
	if (retimg){
		img = readNIfTI(outfile, reorient= FALSE)
		return(img)
	}
	return(invisible(NULL))
}


