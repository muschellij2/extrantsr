\name{my.tab}
\alias{my.tab}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
my.tab(x, y, dnames = c("x", "y"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{dnames}{
%%     ~~Describe \code{dnames} here~~
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
function (x, y, dnames = c("x", "y")) 
{
    x = as.numeric(x)
    y = as.numeric(y)
    stopifnot(all(unique(c(x, y)) \%in\% c(0, 1, NA)))
    tt = sum(x * y)
    t1 = sum(x)
    t2 = sum(y)
    tab = matrix(c(length(x) - t1 - t2 + tt, t1 - tt, t2 - tt, 
        tt), 2, 2)
    n = list(c("FALSE", "TRUE"), c("FALSE", "TRUE"))
    names(n) = dnames
    dimnames(tab) = n
    tab = as.table(tab)
    return(tab)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
