
#' @title Quick Tabulation for logical vectors
#'
#' @description Speeds up on table for logical x and y
#' @param x Logical or 0/1 vector
#' @param y Logical or 0/1 vector
#' @param dnames names for table
#' @return table of x vs y
my.tab <- function(
  x, 
  y, 
  dnames=c("x", "y")) {
  x = as.numeric(x)
  y = as.numeric(y)
  stopifnot(all(unique(c(x,y)) %in% c(0, 1, NA)))
  tt = sum(x * y)
  t1=sum(x)
  t2=sum(y)
  tab = matrix(c(length(x)-t1-t2+tt,  t1-tt, t2-tt, tt), 2, 2)
  n = list(c("FALSE", "TRUE"), c("FALSE", "TRUE"))
  names(n) = dnames
  dimnames(tab) = n
  tab = as.table(tab)
  return(tab) 
}



#' @title Calculate Overlap Metrics
#'
#' @description Calculates perofrmance metrics, like dice, Jaccard,
#' sensitivity, specificity
#' @param dman vector of manually delineated values
#' @param dauto vector of automatically delineated values
#' @return List of performance measures
sim <-  function(
  dman, # vector of manually delineated values
  dauto # vector of automatically delineated values
){
  dman = c(dman)
  dauto = c(dauto)
  N = length(dman)
  N_auto = length(dauto)
  stopifnot( N == N_auto)
  
  stopifnot( ! any(is.na(dman)) )
  stopifnot( ! any(is.na(dauto)) )
  
  # system.time({
  #   tt <- sum( dman &  dauto)
  #   tf <- sum( dman & !dauto)
  #   ft <- sum(!dman &  dauto)
  #   ff <- sum(!dman & !dauto)
  #   tab = matrix(c(ff, tf, ft, tt), ncol=2)
  #   colnames(tab) = rownames(tab) = c("FALSE", "TRUE")
  #   tab
  # })
  
  tab = my.tab(dman, dauto, dnames = c("dman", "dauto"))
  tt = tab["TRUE", 'TRUE']
  
  ptab = prop.table(tab)
  rowtab = prop.table(tab, 1)
  coltab = prop.table(tab, 2)
  
  accur = sum(diag(ptab))
  sens = rowtab["TRUE", "TRUE"]
  spec = rowtab["FALSE", "FALSE"]
  
  
  ab <- tt
  # estvol = sum(dauto)
  # truevol = sum(dman)
  
  estvol  = sum(tab[, "TRUE"])
  truevol = sum(tab["TRUE", ])
  
  aplusb <- (estvol + truevol)
  # aorb <- sum(dman | dauto)
  aorb = sum(tab) - tab["FALSE", "FALSE"]
  dice <- 2 * ab/aplusb
  jaccard <- ab/aorb 
  
  
  # tab <- table(cdman, cdauto, dnn=c("dman", "dauto"))
  res <- list(dice=dice, jaccard=jaccard, 
              sens=sens, spec = spec, accur=accur, truevol = truevol,
              estvol = estvol)
  cat("\n")
  print(res)
  return(res)
}




#' @title Non-exported function to make png
#'
#' @description Make my png
#' @param outfile Filename to be added to for png
#' @param addstub extension to add to outfile
#' @param ... arguments passed to \code{\link{png}}
#' @return Alpha level depending on device
makepng <- function(
  outfile,
  addstub, # extension to add to outfile
  ... # arguments passed to \code{\link{png}}
){
  pngname = paste0(outfile, "_", addstub, ".png")
  print(pngname)
  png(pngname, type="cairo", ...)
  
  myalpha = 0.25
  if (names(dev.cur())[1] == "X11"){
    myalpha = 1
  }
  myalpha
}

