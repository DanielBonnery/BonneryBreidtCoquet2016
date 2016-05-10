\name{simulation.summary}
\alias{simulation.summary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
simulation.summary(table_data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{table_data}{
%%     ~~Describe \code{table_data} here~~
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
function (table_data) 
{
    lapply(table_data, function(l) {
        ll <- c(l$sim[c("Mean", "Bias", "Variance", "M.S.E.", 
            "E")], l$cave["V"])
        X <- do.call(rbind, lapply(c("Naive", "Pseudo", "Sample", 
            "Full"), function(est) {
            do.call(data.frame, c(list(Estimator = est), list(theta = as.array(list(l$model$theta))), 
                list(xi = as.array(list(l$model$xi))), list(`Contidional to` = as.array(list(l$model$conditionalto))), 
                list(Mean = as.array(list(signif(ll$Mean[est][[1]], 
                  3)))), list(`\% Relative Bias` = as.array(list(signif(100 * 
                  ll$Bias[est][[1]]/ll$E[est][[1]], 3)))), list(`RMSE Ratio` = as.array(list(signif(diag(as.matrix(ll$M.S.E.[est][[1]]))/diag(as.matrix(ll$M.S.E["Sample"][[1]], 
                  3)))))), list(`Empirical Variance` = as.array(list(signif(diag(as.matrix(ll$Variance[est][[1]], 
                  3)))))), list(`Asymptotic Variance` = as.array(list(signif(diag(as.matrix(ll$V[est][[1]], 
                  3))))))))
        }))
        names(X) <- c("Estimator", "$\\theta$", "$\\xi$", "Conditional to", 
            "Mean", "\% Relative Bias", "RMSE Ratio", "Empirical Variance", 
            "Asymptotic Variance")
        X
    })
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
