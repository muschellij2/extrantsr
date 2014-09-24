\name{ants.overlay}
\alias{ants.overlay}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ants.overlay(img, oimg, slices = 53, col.x = gray(0:64/64), col.y = "#FF0000FF", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{img}{
%%     ~~Describe \code{img} here~~
}
  \item{oimg}{
%%     ~~Describe \code{oimg} here~~
}
  \item{slices}{
%%     ~~Describe \code{slices} here~~
}
  \item{col.x}{
%%     ~~Describe \code{col.x} here~~
}
  \item{col.y}{
%%     ~~Describe \code{col.y} here~~
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
function (img, oimg, slices = 53, col.x = gray(0:64/64), col.y = "#FF0000FF", 
    ...) 
{
    oldpar <- par(no.readonly = TRUE)
    lslice = length(slices)
    par(mfrow = c(1, lslice), mar = rep(0, 4), bg = "black")
    for (islice in seq(lslice)) {
        slice = slices[islice]
        image(as.array(img)[, , slice], col = col.x, ...)
        oslice = as.array(oimg)[, , slice]
        oslice[oslice == 0] = NA
        image(oslice, col = col.y, add = TRUE)
    }
    par(oldpar)
    invisible()
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
