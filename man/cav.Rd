\name{cav}
\alias{cav}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cav(model, N, nbrepSigma = 300, nbrepI = 300, conditionalto = NULL, methodI = "MC", methodSigma = "MC")
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
  \item{nbrepI}{
%%     ~~Describe \code{nbrepI} here~~
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
function (model, N, nbrepSigma = 300, nbrepI = 300, conditionalto = NULL, 
    methodI = "MC", methodSigma = "MC") 
{
    Sigma <- calcule.Sigma(model, N, nbrepSigma, conditionalto = conditionalto)
    Imatrix <- Imatrix7(N, model, conditionalto = conditionalto)
    dimtheta <- length(model$theta)
    V123 <- calculeV(Sigma, Imatrix, dimtheta)
    V <- list(Sample = V123$V/(N * model$tau), Pseudo = NA, Naive = NA, 
        Full = NA)
    return(list(Sigma = Sigma, Im = Imatrix, V = V, V1 = V123$V1/(N * 
        model$tau), V2 = V123$V2/(N * model$tau), V3 = V123$V3/(N * 
        model$tau)))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
