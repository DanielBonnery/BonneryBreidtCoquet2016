\name{sampleMLE}
\alias{sampleMLE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
Computes the sample MLE 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{sampleMLE(y, z, s, model, method = "nlm", xi.hat = NULL)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{z}{
%%     ~~Describe \code{z} here~~
}
  \item{s}{
%%     ~~Describe \code{s} here~~
}
  \item{model}{
%%     ~~Describe \code{model} here~~
}
  \item{method}{
%%     ~~Describe \code{method} here~~
}
  \item{xi.hat}{
%%     ~~Describe \code{xi.hat} here~~
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
function (y, z, s, model, method = "nlm", xi.hat = NULL) 
{
    if (method == "formula") {
        model$sampleMLE(y, z, s)
    }
    else {
        if (is.null(xi.hat)) {
            xi.hat <- model$xihat(y, z, s, model$Scheme$Pik(z))
        }
        unlist(optimx::optimx(model$theta, fn = sample.loglikelihood.plugin, 
            method = method, control = list(maximize = TRUE), 
            y = as.matrix(y)[s, ], model = model, xi = xi.hat))[1:length(model$theta)]
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
