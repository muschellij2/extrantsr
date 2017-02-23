# rm(list= ls())
# library(extrantsr)
# library(fslr)
# library(R.matlab)
# library(ANTsR)
# fname = '/Users/johnmuschelli/Dropbox/CTR/DHanley/CT_Registration/Seg_Release_data/seg_release_paper/seg_release_paper/TCGA-GBM/Head_NC_Only/ss/Tcga140789_TCGA140789_0_2213570360326183_19971121_Routine_Head_221636.nii.gz'
# # img = readnii(fname)
# img[ img < 0 ] = 0

img_fname = path.expand("~/Desktop/img.nii")
# img = readnii(img_fname)
flipped_fname = path.expand("~/Desktop/img_flip.nii")
# flipped = readnii()
img = img_fname
flipped = flipped_fname

# mid_sagittal_align = function(img) {
  
  interpolator = "Linear"
  rp = rpi_orient_file(img)
  # 
  img = rp$img
  img = check_nifti(img)
  # dd = dropEmptyImageDimensions(
  #   img > 0,
  #   other.imgs = img)
  # img = dd$other.imgs
  # 
  # flip_lr = function(x){
  #   fsl_swapdim(file = x, a = "-x")
  # }
  # 
  # flipped = flip_lr(img)

  reg = list(fwdtransforms = "~/Desktop/file612172387d0f0GenericAffine.mat")  
  reg$fwdtransforms = path.expand(reg$fwdtransforms)
  # setAntsrTransformParameters
  # createAntsrTransform
  # reg = registration(filename = img,
  #                    skull_strip = FALSE,
  #                    correct = FALSE,
  #                    template.file = flipped,
  #                    typeofTransform = "Rigid",
  #                    interpolator = interpolator)
    
  amat = readAntsrTransform(reg$fwdtransforms)
  
  params = getAntsrTransformParameters(amat)
  params = params / 2
  
  fixed = getAntsrTransformFixedParameters(amat)
  fixed = fixed / 2
  
  trans = createAntsrTransform(
    parameters = params)
  
  # amat2 = amat
  # amat2$AffineTransform.float.3.3 = 
  #   amat2$AffineTransform.float.3.3 / 2
  # tfile = tempfile(fileext = ".mat")
  # writeMat(
  #   AffineTransform_float_3_3 = amat2$AffineTransform.float.3.3, 
  #   fixed = amat2$fixed,
  #   matVersion = attr(amat2, "header")$version,
  #   con = tfile)
  
  tfile = tempfile(fileext = ".mat")
  writeAntsrTransform(trans, filename = tfile)
  output = ants_apply_transforms(
    fixed= img, 
    moving = flipped, 
    transformlist = tfile)
  
  # 
  # 
  # amat = amat$AffineTransform.float.3.3
  # amat2$AffineTransform.float.3.3[]
  # P = amat / 2
  # d4 = diag(4)
  # R1 = d4
  # R1[2,2] = cos(P[4]);
  # R1[2,3] = sin(P[4])
  # R1[3,2] = -sin(P[4]);
  # R1[3,3] = cos(P[4])  
  # 
  # R2 = d4
  # R2[1,1] = cos(P[5]);
  # R2[1,3] = sin(P[5])
  # R2[3,1] = -sin(P[5]);
  # R2[3,3] = cos(P[5])  
  # 
  # R3 = d4
  # R3[1,1] = cos(P[6]);
  # R3[1,2] = sin(P[6])
  # R3[2,1] = -sin(P[6]);
  # R3[2,2] = cos(P[6])
  # 
  # R   = R1 %*% R2 %*% R3;
  
  # amat = matrix(amat, ncol = 4, byrow = FALSE)
  # 
  omat = tempfile(fileext = ".mat")
  outfile = tempfile(fileext = ".nii.gz")
  ff = flirt(infile = img, 
             reffile = flipped,
             omat = omat, dof = 6,
             retimg = FALSE, outfile = outfile)  
  
  
  
  mat = readLines(omat)
  mat = trimws(mat)
  mat = strsplit(mat, " ")
  mat = lapply(mat, function(x) x[ !x %in% ""])
  mat = lapply(mat, as.numeric)
  mat = do.call("rbind", mat)
  xmat = mat
  
  # library(expm)
  # A = xmat[1:3, 1:3]
  # AtA = t(A) %*% A
  # AtA[ abs(AtA) < 1e-8] = 0
  # S = solve(A %*% t(A))
  # sqrtAtA = sqrtm(AtA)
  # R = solve(sqrtAtA) %*% A
  # A2 = S%*% R*0.5
  # newmat = xmat
  # newmat[1:3,4] = xmat[1:3,4] * 0.5
  # newmat[1:3, 1:3] = A2
  
  
  scaled = fsl_avscale(file = omat)
  parsed = parse_avscale(scaled)
  
  mat = parsed$fwd_half_transform
  # mat = mat * 0.5
  
  new_omat = tempfile(fileext = ".mat")
  mat = apply(mat, 1, paste, collapse = " ")
  mat = paste0(mat, " ")
  writeLines(mat, new_omat)
  
  centered = flirt_apply(infile = img, reffile = flipped, 
                         initmat = new_omat)
  
  
# }