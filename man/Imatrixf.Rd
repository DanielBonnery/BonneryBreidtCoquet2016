\name{Imatrixf}
\alias{Imatrixf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Imatrixf(N, model, nbrepI = 300, methodIm = "FirstDeriv")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{N}{
%%     ~~Describe \code{N} here~~
}
  \item{model}{
%%     ~~Describe \code{model} here~~
}
  \item{nbrepI}{
%%     ~~Describe \code{nbrepI} here~~
}
  \item{methodIm}{
%%     ~~Describe \code{methodIm} here~~
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
function (N, model, nbrepI = 300, methodIm = "FirstDeriv") 
{
    if (methodIm == "formula" & !is.null(model$cave$Imatrix)) {
        model$cave$Imatrix
    }
    if (methodIm == "FirstDeriv" & !is.null(model$cave$Imatrix)) {
        model$cave$Imatrix
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
