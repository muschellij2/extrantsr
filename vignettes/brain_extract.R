## ----setup, include=FALSE------------------------------------------------
library(kirby21.fmri)
library(kirby21.base)
library(fslr)
library(neurobase)
library(extrantsr)
knitr::opts_chunk$set(echo = TRUE, comment = "")

## ---- eval = FALSE-------------------------------------------------------
#  packages = installed.packages()
#  packages = packages[, "Package"]
#  if (!"kirby21.base" %in% packages) {
#    devtools::install_github("muschellij2/kirby21.base")
#  }
#  if (!"kirby21.t1" %in% packages) {
#    devtools::install_github("muschellij2/kirby21.t1")
#  }

## ----data----------------------------------------------------------------
library(kirby21.t1)
library(kirby21.base)
fnames = get_image_filenames_df(ids = 113, 
                    modalities = c("T1"), 
                    visits = c(1),
                    long = FALSE)
t1_fname = fnames$T1[1]

## ----t1_plot-------------------------------------------------------------
t1 = readnii(t1_fname)
ortho2(t1)

## ----t1_naive_ss, cache = FALSE------------------------------------------
if (have.fsl()) {
  ss_naive = fslbet(infile = t1_fname)
}

## ----t1_naive_plot, cache = FALSE----------------------------------------
if (have.fsl()) {
  ortho2(ss_naive)
}


## ----t1_ss, cache = FALSE------------------------------------------------
n4img = bias_correct(t1_fname, 
                     correction = "N4", retimg = TRUE,
                     verbose = FALSE)
if (have.fsl()) {
  template.file = file.path(fsldir(), "data/standard",
                            "MNI152_T1_1mm_brain.nii.gz") 
  template.mask = file.path(fsldir(),
                            "data/standard", "MNI152_T1_1mm_brain_mask.nii.gz")  
  removed_neck = extrantsr::double_remove_neck(
    n4img,
    template.file = template.file,
    template.mask = template.mask)
  ortho2(removed_neck)
  double_ortho(t1, removed_neck)
}

## ------------------------------------------------------------------------
if (have.fsl()) {
  ss = extrantsr::fslbet_robust(t1_fname, 
    remover = "double_remove_neck")
  ortho2(ss)
}

## ----t1_ss_plot2, cache = FALSE------------------------------------------
if (have.fsl()) {
  alpha = function(col, alpha = 1) {
    cols = t(col2rgb(col, alpha = FALSE)/255)
    rgb(cols, alpha = alpha)
  }      
  ortho2(t1_fname, ss > 0, col.y = alpha("red", 0.5))
}

## ----t1_ss_red, cache = FALSE--------------------------------------------
if (have.fsl()) {
  ss_red = dropEmptyImageDimensions(ss)
  ortho2(ss_red)
}

