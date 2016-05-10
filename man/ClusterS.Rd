\name{ClusterS}
\alias{ClusterS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ClusterS(sampleparam)
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
        oo <- order(z)
        N = length(z)
        Nh <- vector()
        nh <- vector()
        Pikk <- rep(NULL, length(z))
        cum = 0
        for (i in 1:(length(sampleparam$proph) - 1)) {
            Nh[i] <- floor(sampleparam$proph[i] * N)
            nh[i] <- floor(sampleparam$tauh[i] * Nh[i])
            Pikk[oo > cum & oo <= cum + Nh[i]] <- sampleparam$proph[i] * 
                (nh[i]/Nh[i])
            cum <- cum + Nh[i]
        }
        Nh[i + 1] <- N - sum(Nh)
        nh[i + 1] <- floor(sampleparam$tauh[i + 1] * Nh[i + 1])
        Pikk[oo > cum] <- sampleparam$proph[i] * (nh[i + 1]/Nh[i + 
            1])
        return(Pikk)
    }, S = function(z) {
        oo <- order(z)
        N = length(z)
        nbclus <- length(sampleparam$proph)
        clus <- sample(1:nbclus, size = 1, prob = sampleparam$probh, 
            replace = FALSE)
        cum <- floor((sum(sampleparam$proph[1:clus]) - sampleparam$proph[clus]) * 
            N)
        Nh <- floor(sampleparam$proph[clus] * N)
        S = sample((1:N)[oo > cum & oo <= cum + Nh], size = floor(sampleparam$tauh[clus] * 
            Nh), replace = FALSE)
        return(S)
    }, demarc = function(z) {
        oo <- order(z)
        N = length(z)
        dem <- vector()
        Nh <- vector()
        cum <- 0
        for (i in 1:(length(sampleparam$proph) - 1)) {
            Nh[i] <- floor(sampleparam$proph[i] * N)
            cum <- cum + Nh[i]
            dem[i] <- z[oo == cum]
        }
        return(dem)
    })
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
