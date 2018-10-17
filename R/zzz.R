rule <- function(pad = "-", gap = 2L) {
  paste0(rep(pad, getOption("width") - gap), collapse = "")
}

.onAttach <- function(lib, pkg) {
  setHook(packageEvent("oro.nifti", "attach"), function(...) {
    packageStartupMessage(rule())
    packageStartupMessage(
      "You have loaded oro.nifti after extrantsr ", 
      "(either directly or from another package) - this is likely ",
      "to cause problems with certain functions on antsImage types", 
      ", such as origin.\n", 
      "If you need functions from both extrantsr and oro.nifti, ",
      "please load oro.nifti first, then extrantsr:\n", 
      "library(oro.nifti); library(extrantsr)"
    )
    packageStartupMessage(rule())

  })
}



.onDetach <- function(libpath) {
  setHook(packageEvent("oro.nifti", "attach"), NULL, "replace")
}