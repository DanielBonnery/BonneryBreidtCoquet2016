\name{StratS}
\alias{StratS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
StratS(sampleparam)
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
        N = length(z)
        Nh <- floor(sampleparam$proph * N)
        Nh[length(Nh)] = Nh[length(Nh)] + N - sum(Nh)
        nh <- floor(sampleparam$tauh * Nh)
        Pikk <- rep(nh/Nh, Nh)[rank(z)]
        return(Pikk)
    }, S = function(z) {
        N = length(z)
        Nh <- floor(sampleparam$proph * N)
        Nh[length(Nh)] = Nh[length(Nh)] + N - sum(Nh)
        nh <- floor(sampleparam$tauh * Nh)
        unlist(sapply(1:length(Nh), function(i) {
            sample((1:N)[(rank(z) > c(0, cumsum(Nh))[i]) & (rank(z) <= 
                cumsum(Nh)[i])], nh[i])
        }))
    }, demarc = function(z) {
        N = length(z)
        Nh <- floor(sampleparam$proph * N)
        Nh[length(Nh)] = Nh[length(Nh)] + N - sum(Nh)
        z[order(z)][cumsum(Nh)[-length(Nh)]]
    })
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
