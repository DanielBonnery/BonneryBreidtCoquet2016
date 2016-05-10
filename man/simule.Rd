\name{simule}
\alias{simule}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
simule(N, model, nbreps = 300, method = list(Sample = "nlm", Pseudo = "nlm", Naive = "nlm", Full = "nlm"), conditionalto = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{N}{
%%     ~~Describe \code{N} here~~
}
  \item{model}{
%%     ~~Describe \code{model} here~~
}
  \item{nbreps}{
%%     ~~Describe \code{nbreps} here~~
}
  \item{method}{
%%     ~~Describe \code{method} here~~
}
  \item{conditionalto}{
%%     ~~Describe \code{conditionalto} here~~
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
function (N, model, nbreps = 300, method = list(Sample = "nlm", 
    Pseudo = "nlm", Naive = "nlm", Full = "nlm"), conditionalto = NULL) 
{
    attach(model)
    attach(Scheme)
    Estim <- plyr::rlply(nbreps, (function() {
        Yg <- rloiy(N, conditionalto)
        Zg <- rloiz(Yg)
        Sg <- S(Zg)
        Pikg <- Pik(Zg)
        thetaxi.full = fullMLE(Yg, Zg, Sg, model, method)
        xi.hat = xihat(Yg, Zg, Sg, Pikg)
        return(list(xi.hat = xi.hat, Pseudo = thetaht(Yg, Zg, 
            Sg, Pikg), Naive = thetaniais(Yg, Zg, Sg), Sample = try(sampleMLE(Yg, 
            Zg, Sg, model, method, xi.hat = xi.hat)), xi.full = thetaxi.full[length(model$theta) + 
            (1:length(model$xi))], Full = thetaxi.full[1:length(model$theta)]))
    })())
    noms <- names(Estim[[1]])
    Estim <- Estim[sapply(Estim, function(l) {
        !is.character(l$Sample)
    })]
    Estim <- lapply(as.list(noms), function(nom) {
        plyr::laply(Estim, function(ll) {
            ll[[nom]]
        })
    })
    names(Estim) <- noms
    attach(Estim)
    Var <- lapply(Estim, var, na.rm = TRUE)
    M = lapply(Estim, function(est) {
        apply(as.matrix(est), 2, mean, na.rm = TRUE)
    })
    E = list(model$xi, model$theta, model$theta, model$theta, 
        model$xi, model$theta)
    names(E) <- names(M)
    Bias = lapply(as.list(noms), function(x) {
        M[[x]] - E[[x]]
    })
    names(Bias) <- names(M)
    MSE = lapply(as.list(noms), function(x) {
        Var[[x]] + Bias[[x]] \%*\% t(Bias[[x]])
    })
    names(MSE) <- names(M)
    return(list(model = model, Estim = Estim, Variance = Var, 
        Mean = M, Bias = Bias, E = E, M.S.E. = MSE))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
