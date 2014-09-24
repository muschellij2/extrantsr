\name{sim}
\alias{sim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sim(dman, dauto, dim.dman, dim.dauto)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dman}{
%%     ~~Describe \code{dman} here~~
}
  \item{dauto}{
%%     ~~Describe \code{dauto} here~~
}
  \item{dim.dman}{
%%     ~~Describe \code{dim.dman} here~~
}
  \item{dim.dauto}{
%%     ~~Describe \code{dim.dauto} here~~
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
function (dman, dauto, dim.dman, dim.dauto) 
{
    if (!all(dim.dman == dim.dauto)) 
        stop("Wrong Dimensions")
    N <- prod(dim.dman)
    stopifnot(!any(is.na(dman)))
    stopifnot(!any(is.na(dauto)))
    tab = my.tab(dman, dauto, dnames = c("dman", "dauto"))
    tt = tab["TRUE", "TRUE"]
    ptab = prop.table(tab)
    rowtab = prop.table(tab, 1)
    coltab = prop.table(tab, 2)
    accur = sum(diag(ptab))
    sens = rowtab["TRUE", "TRUE"]
    spec = rowtab["FALSE", "FALSE"]
    ab <- tt
    estvol = sum(tab[, "TRUE"])
    truevol = sum(tab["TRUE", ])
    aplusb <- (estvol + truevol)
    aorb = sum(tab) - tab["FALSE", "FALSE"]
    dice <- 2 * ab/aplusb
    jaccard <- ab/aorb
    res <- list(dice = dice, jaccard = jaccard, sens = sens, 
        spec = spec, accur = accur, truevol = truevol, estvol = estvol)
    cat("\n")
    print(res)
    return(res)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
