\name{t1_syn}
\alias{t1_syn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
t1_syn(filename, n3correct = FALSE, retimg = TRUE, outfile = NULL, interpolator = "Linear", other.files = NULL, other.outfiles = NULL, native.cereb = TRUE, native.fname = NULL, atlas.file = NULL, typeofTransform = "SyN", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
%%     ~~Describe \code{filename} here~~
}
  \item{n3correct}{
%%     ~~Describe \code{n3correct} here~~
}
  \item{retimg}{
%%     ~~Describe \code{retimg} here~~
}
  \item{outfile}{
%%     ~~Describe \code{outfile} here~~
}
  \item{interpolator}{
%%     ~~Describe \code{interpolator} here~~
}
  \item{other.files}{
%%     ~~Describe \code{other.files} here~~
}
  \item{other.outfiles}{
%%     ~~Describe \code{other.outfiles} here~~
}
  \item{native.cereb}{
%%     ~~Describe \code{native.cereb} here~~
}
  \item{native.fname}{
%%     ~~Describe \code{native.fname} here~~
}
  \item{atlas.file}{
%%     ~~Describe \code{atlas.file} here~~
}
  \item{typeofTransform}{
%%     ~~Describe \code{typeofTransform} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (filename, n3correct = FALSE, retimg = TRUE, outfile = NULL, 
    interpolator = "Linear", other.files = NULL, other.outfiles = NULL, 
    native.cereb = TRUE, native.fname = NULL, atlas.file = NULL, 
    typeofTransform = "SyN", ...) 
{
    require(fslr)
    require(ANTsR)
    stopifnot(have.fsl())
    writeFile = FALSE
    if (retimg) {
        if (is.null(outfile)) {
            outfile = paste0(tempfile(), ".nii.gz")
        }
        else {
            writeFile = TRUE
        }
    }
    else {
        stopifnot(!is.null(outfile))
    }
    have.other = FALSE
    if (!is.null(other.files)) {
        have.other = TRUE
        lother = length(other.files)
        lout = length(other.outfiles)
        if (lother != lout) {
            stopifnot("Other outfile and infiles must be same length")
        }
    }
    if (native.cereb) {
        stopifnot(!is.null(native.fname))
    }
    t1 <- antsImageRead(filename, 3)
    t1N3 <- antsImageClone(t1)
    if (have.other) {
        other.imgs = lapply(other.files, antsImageRead, dimension = 3)
        N3.oimgs = lapply(other.imgs, antsImageClone)
    }
    if (n3correct) {
        N3BiasFieldCorrection(t1@dimension, t1, t1N3, "4")
        if (have.other) {
            for (i in seq(lother)) {
                N3BiasFieldCorrection(other.imgs[[i]]@dimension, 
                  other.imgs[[i]], N3.oimgs[[i]], "4")
            }
        }
    }
    tdir = file.path(fsldir(), "data", "standard")
    template.path <- file.path(tdir, "MNI152_T1_1mm_brain.nii.gz")
    template <- antsImageRead(template.path, 3)
    stopifnot(!is.null(atlas.file))
    outprefix = tempfile()
    antsRegOut.nonlin <- antsRegistration(fixed = template, moving = t1N3, 
        typeofTransform = typeofTransform, outprefix = outprefix)
    t1.to.template <- antsApplyTransforms(fixed = template, moving = t1N3, 
        transformlist = antsRegOut.nonlin$fwdtransforms, interpolator = interpolator)
    moving = t1N3
    transformlist = antsRegOut.nonlin$invtransforms
    dimension = 3
    output = paste0(tempfile(), ".nii.gz")
    if (native.cereb) {
        for (iatlas in seq_along(atlas.file)) {
            output = native.fname[iatlas]
            atlas.img <- readNIfTI(atlas.file[iatlas], reorient = FALSE)
            atlas.img = cal_img(atlas.img > 0)
            temp.atlas = tempimg(atlas.img)
            fixed = temp.atlas
            if (!grepl("[.]nii$|[.]nii[.]gz$", output)) {
                output = paste0(output, ".nii.gz")
            }
            invwarp(3, fixed = fixed, output = output, moving = moving, 
                transformlist = transformlist)
        }
    }
    if (have.other) {
        reg.oimgs = lapply(N3.oimgs, function(x) {
            antsApplyTransforms(fixed = template, moving = x, 
                transformlist = antsRegOut.nonlin$fwdtransforms, 
                interpolator = interpolator)
        })
    }
    antsImageWrite(t1.to.template, outfile)
    if (have.other) {
        for (i in seq(lother)) {
            antsImageWrite(reg.oimgs[[i]], other.outfiles[i])
        }
    }
    file.remove(paste0(outprefix, "1InverseWarp.nii.gz"), showWarnings = FALSE)
    file.remove(paste0(outprefix, "1Warp.nii.gz"), showWarnings = FALSE)
    if (retimg) {
        img = readNIfTI(outfile, reorient = FALSE)
        return(img)
    }
    return(invisible(NULL))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
