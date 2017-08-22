#' @rdname accessor-methods
#' @title Extract Image Attributes \code{origin} from \code{antsImage} or \code{nifti}
#' objects
#' @description Methods that act on the \code{slots} of the
#' \code{antsImage} or \code{nifti} object
#' @docType methods 
#' @param object is an object of class \code{antsImage} or \code{nifti}.
#' @param value is the value to assign to the slot.  
#' 
#' @importFrom ANTsRCore origin "origin<-"
#' @export
setGeneric("origin", getGeneric("origin", package = "oro.nifti"))

#' @rdname accessor-methods
#' @export
setMethod("origin", "antsImage", function(object) { 
  ANTsRCore::origin(object)
})

#' @rdname accessor-methods
#' @export
setMethod("origin", "nifti", function(object) { 
  oro.nifti::origin(object)
})

#' @rdname accessor-methods
#' @export
setMethod("origin", "anlz", function(object) { 
  oro.nifti::origin(object)
})

#' @rdname accessor-methods
#' @export
setGeneric("origin<-", getGeneric("origin<-", package = "oro.nifti"))

#' @rdname accessor-methods
#' @export
setMethod("origin<-", 
          signature(object = "antsImage"), 
          function(object, value) { 
            object = ANTsRCore::`origin<-`(object, value)
            return(object)
          })

#' @rdname accessor-methods
#' @export
setMethod("origin<-", 
          signature(object = "nifti"), 
          function(object, value) { 
            object = oro.nifti::`origin<-`(object, value)
            return(object)
          })

#' @rdname accessor-methods
#' @export
setMethod("origin<-", 
          signature(object = "anlz"), 
          function(object, value) { 
            object = oro.nifti::`origin<-`(object, value)
            return(object)
          })





