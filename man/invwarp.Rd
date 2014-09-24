\name{invwarp}
\alias{invwarp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
invwarp(dimension, fixed, output, moving, transformlist)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dimension}{
%%     ~~Describe \code{dimension} here~~
}
  \item{fixed}{
%%     ~~Describe \code{fixed} here~~
}
  \item{output}{
%%     ~~Describe \code{output} here~~
}
  \item{moving}{
%%     ~~Describe \code{moving} here~~
}
  \item{transformlist}{
%%     ~~Describe \code{transformlist} here~~
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
function (dimension, fixed, output, moving, transformlist) 
{
    moving = tempants(moving)
    fixed = tempants(fixed)
    output = tempants(output)
    args <- list(dimension, fixed, output, R = moving, i = transformlist)
    myargs <- int_antsProcessArguments(c(args))
    myargs = myargs[myargs != "-"]
    cmd = paste0("WarpImageMultiTransform ", paste(myargs, collapse = " "))
    system(cmd)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
