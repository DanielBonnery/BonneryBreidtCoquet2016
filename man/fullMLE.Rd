\name{fullMLE}
\alias{fullMLE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fullMLE(y, z, s, model, method = "nlm")
}
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
function (y, z, s, model, method = "nlm") 
{
    if (is.vector(y)) {
        ys <- y[s]
    }
    if (is.matrix(y)) {
        ys <- y[s, ]
    }
    if (method == "formula") {
        model$fullMLE(y, z, s)
    }
    else {
        if (!is.null(model$fulllikelihood)) {
            optimx::optimx(c(model$theta, model$xi), fn = full.loglikelihood, 
                control = list(maximize = TRUE, method = "nlm"), 
                model = model, y = ys, z = z)
        }
        else {
            NA
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
