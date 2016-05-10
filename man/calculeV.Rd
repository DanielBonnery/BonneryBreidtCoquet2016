\name{calculeV}
\alias{calculeV}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
calculeV(Sigma, Im, dimtheta)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Sigma}{
%%     ~~Describe \code{Sigma} here~~
}
  \item{Im}{
%%     ~~Describe \code{Im} here~~
}
  \item{dimtheta}{
%%     ~~Describe \code{dimtheta} here~~
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
function (Sigma, Im, dimtheta) 
{
    dimxi <- length(Sigma[1, ]) - dimtheta
    Sigma11 <- Sigma[1:dimtheta, 1:dimtheta]
    Sigma22 <- Sigma[dimtheta + dimxi, dimtheta + dimxi]
    Sigma12 <- Sigma[1:dimtheta, dimtheta + dimxi]
    I11 <- Im[1:dimtheta, 1:dimtheta]
    I12 <- as.matrix(Im[1:dimtheta, dimtheta + dimxi])
    V <- solve(I11) \%*\% (Sigma11 + I12 \%*\% Sigma22 \%*\% t(I12) - 
        I12 \%*\% t(Sigma12) - Sigma12 \%*\% t(I12)) \%*\% solve(I11)
    V1 <- solve(I11) \%*\% (Sigma11) \%*\% solve(I11)
    V2 <- solve(I11) \%*\% (I12 \%*\% Sigma22 \%*\% t(I12)) \%*\% solve(I11)
    V3 <- solve(I11) \%*\% (-I12 \%*\% t(Sigma12) - Sigma12 \%*\% t(I12)) \%*\% 
        solve(I11)
    return(list(V = V, V1 = V1, V2 = V2, V3 = V3))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
