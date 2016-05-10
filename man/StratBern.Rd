\name{StratBern}
\alias{StratBern}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
StratBern(sampleparam)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sampleparam}{
%%     ~~Describe \code{sampleparam} here~~
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
function (sampleparam) 
{
    list(sampleparam = sampleparam, Pik = function(z) {
        ooo = sort(unique(z))
        Pikk <- rep(NULL, length(z))
        for (i in 1:(length(ooo))) {
            Pikk[z == ooo[i]] <- sampleparam$tauh[i]
        }
        return(Pikk)
    }, S = function(z) {
        ooo = sort(unique(z))
        N <- length(z)
        Pikk <- rep(NULL, length(z))
        for (i in 1:(length(ooo))) {
            Pikk[z == ooo[i]] <- sampleparam$tauh[i]
        }
        return((1:N)[runif(N) < Pikk])
    }, Sind = function(z) {
        ooo = sort(unique(z))
        N <- length(z)
        Pikk <- rep(NULL, length(z))
        for (i in 1:(length(ooo))) {
            Pikk[z == ooo[i]] <- sampleparam$tauh[i]
        }
        return(runif(N) < Pikk)
    }, demarc = function(z) {
        return(sort(unique(z)))
    })
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
