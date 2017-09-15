## ----setup, include=FALSE------------------------------------------------
library(kirby21.t1)
library(fslr)
library(neurobase)
library(extrantsr)
knitr::opts_chunk$set(echo = TRUE, comment = "")

## ---- eval = FALSE-------------------------------------------------------
#  packages = installed.packages()
#  packages = packages[, "Package"]
#  if (!"kirby21.t1" %in% packages) {
#    devtools::install_github("muschellij2/kirby21.t1")
#  }

## ----data----------------------------------------------------------------
library(kirby21.t1)
download_t1_data()
t1_fname = get_t1_filenames(
  ids = 113, 
  visits = 1)

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


