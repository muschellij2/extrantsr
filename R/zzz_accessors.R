#' @rdname accessor-methods
#' @title Extract Image Attributes \code{origin} from \code{antsImage} or \code{nifti}
#' objects
#' @description Methods that act on the \code{slots} of the
#' \code{antsImage} or \code{nifti} object
#' @docType methods 
#' @param object is an object of class \code{antsImage} or \code{nifti}.
#' @param value is the value to assign to the slot.  
#' 
#' @importMethodsFrom oro.nifti origin "origin<-"
#' @importFrom ANTsRCore origin "origin<-"
#' @export
origin = oro.nifti::origin

#' @export
`origin<-` = oro.nifti::`origin<-`

#' @rdname accessor-methods
#' @export
setMethod("origin", "antsImage", function(object) { 
  ANTsRCore::origin(object)
})


#' @rdname accessor-methods
#' @export
setMethod("origin<-", 
          signature(object = "antsImage"), 
          function(object, value) { 
            object = ANTsRCore::`origin<-`(object, value)
            return(object)
          })
