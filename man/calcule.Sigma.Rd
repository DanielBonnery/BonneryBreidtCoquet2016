\name{calcule.Sigma}
\alias{calcule.Sigma}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
calcule.Sigma(model, N, nbrepSigma = 1000, conditionalto = NULL, methodI = "MC", methodSigma = "MC")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
%%     ~~Describe \code{model} here~~
}
  \item{N}{
%%     ~~Describe \code{N} here~~
}
  \item{nbrepSigma}{
%%     ~~Describe \code{nbrepSigma} here~~
}
  \item{conditionalto}{
%%     ~~Describe \code{conditionalto} here~~
}
  \item{methodI}{
%%     ~~Describe \code{methodI} here~~
}
  \item{methodSigma}{
%%     ~~Describe \code{methodSigma} here~~
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
function (model, N, nbrepSigma = 1000, conditionalto = NULL, 
    methodI = "MC", methodSigma = "MC") 
{
    return(N * model$tau * var(plyr::aaply(plyr::raply(nbrepSigma, 
        function() {
            y = model$rloiy()
            z = model$rloiz(y)
            s <- model$Scheme$S(z)
            pi <- model$Scheme$Pik(z)
            return(apply(cbind(Deriveloglikethetaxi(as.matrix(y)[s, 
                ], model, model$theta, model$xi), model$xihatfunc1(as.matrix(y)[s, 
                ], as.matrix(z)[s, ], pi[s])), 2, mean))
        }), 1, function(u) {
        c(u[1:length(model$theta)], model$xihatfunc2(u[length(model$theta) + 
            length(model$xi) + (1:model$xihatfuncdim)]))
    })))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
