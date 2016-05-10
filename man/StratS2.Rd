\name{StratS2}
\alias{StratS2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
StratS2(sampleparam)
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
        oo <- rank(z)
        N = length(z)
        Nh <- as.vector(table(oo))
        nh <- vector()
        Sam <- vector()
        cum <- 0
        for (i in 1:(length(Nh))) {
            nh[i] <- floor(sampleparam$tauh[i] * Nh[i])
            Sam <- c(Sam, sample((1:N)[(oo > cum) & (oo <= cum + 
                Nh[i])], size = nh[i], replace = FALSE))
            cum <- cum + Nh[i]
        }
        return(Sam)
    }, demarc = function(z) {
        oo <- rank(z)
        z <- sort(z)
        N = length(z)
        Nh <- as.vector(table(oo))
        dem <- vector()
        Nh <- vector()
        cum <- 0
        for (i in 1:(length(Nh) - 1)) {
            cum <- cum + Nh[i]
            dem[i] <- sort(z)[cum]
        }
        return(dem)
    })
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
