\name{SystematicPPS}
\alias{SystematicPPS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SystematicPPS(sampleparam)
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
        N <- length(z)
        n <- floor(N * sampleparam$tau)
        return(((n * abs(z)/sum(abs(z))) > 1) + ((n * abs(z)/sum(abs(z))) <= 
            1) * n * abs(z)/sum(abs(z)))
    }, S = function(z) {
        N <- length(z)
        n <- floor(N * sampleparam$tau)
        u <- runif(N)
        oo <- order(u)
        cum <- cumsum(abs(z)[oo])
        if (n > 0) {
            sel <- (runif(1) + (0:(n - 1))) * cum[N]/n
        }
        return(oo[apply(outer(sel, cum, FUN = function(a, b) {
            a < b
        }) & outer(sel, c(0, cum[-N]), FUN = function(a, b) {
            a >= b
        }), 1, FUN = function(x) {
            (1:N)[x]
        })])
    }, demarc = function(z) {
        NULL
    })
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
