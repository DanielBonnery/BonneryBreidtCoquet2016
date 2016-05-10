\name{model.dep.strat2}
\alias{model.dep.strat2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
model.dep.strat2(theta = c(0.5, 1, 1), xi = 2, conditionalto = list(N = 1000, sigma = 1, EX = 1, SX = 1, sampleparam = list(proph = c(0.7, 0.3), tauh = c(1/70, 2/15))))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta}{
%%     ~~Describe \code{theta} here~~
}
  \item{xi}{
%%     ~~Describe \code{xi} here~~
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
function (theta = c(0.5, 1, 1), xi = 2, conditionalto = list(N = 1000, 
    sigma = 1, EX = 1, SX = 1, sampleparam = list(proph = c(0.7, 
        0.3), tauh = c(1/70, 2/15)))) 
{
    attach(conditionalto)
    attach(sampleparam)
    rloiy <- function(.conditionalto = conditionalto) {
        x = qnorm((1:.conditionalto$N)/(.conditionalto$N + 1), 
            .conditionalto$EX, .conditionalto$SX)
        cbind(x, rnorm(x, mean = theta[1] + theta[2] * x, sd = theta[3]))
    }
    rloiz = function(y) {
        rnorm(y[, 2], mean = xi * y[, 2], sd = sigma)
    }
    Scheme <- StratS(sampleparam)
    tau <- sum(proph * tauh)
    Zetah <- qnorm(cumsum(proph), 0, 1)
    zetah <- sqrt(xi^2 * theta[2]^2 * SX^2 + xi^2 * theta[3]^2 + 
        sigma^2) * Zetah + xi * theta[1] + xi * theta[2] * EX
    En <- function(N) {
        tau * N
    }
    dloitheta = function(y, theta) {
        if (is.matrix(y)) {
            return(dnorm(y[, 2], mean = theta[1] + theta[2] * 
                y[, 1], sd = theta[3]))
        }
        if (is.vector(y)) {
            return(dnorm(y[2], mean = theta[1] + theta[2] * y[1], 
                sd = theta[3]))
        }
    }
    dloi = function(y) {
        dloitheta(y, theta)
    }
    rhothetaxi = function(y, theta, xi) {
        if (is.matrix(y)) {
            x <- y[, 1]
            Y <- y[, 2]
        }
        if (is.vector(y)) {
            x <- y[1]
            Y <- y[2]
        }
        y <- Y
        rhorho1 <- tauh[length(tauh)]
        rhorho2 <- tauh[length(tauh)]
        Zetah = qnorm(cumsum(proph), 0, 1)
        zetah = sqrt(xi^2 * theta[2]^2 * SX^2 + xi^2 * theta[3]^2 + 
            sigma^2) * Zetah + xi * theta[1] + xi * theta[2] * 
            EX
        for (h in 1:(length(tauh) - 1)) {
            rhorho1 <- rhorho1 + (tauh[h] - tauh[h + 1]) * pnorm((zetah[h] - 
                xi * y)/sigma)
            rhorho2 <- rhorho2 + (tauh[h] - tauh[h + 1]) * pnorm((zetah[h] - 
                xi * (theta[1] + theta[2] * x))/sqrt(sigma^2 + 
                xi^2 * theta[3]^2))
        }
        return(rhorho1/rhorho2)
    }
    rho = function(y) {
        return(rhothetaxi(y, theta, xi))
    }
    rhoxthetaxi = function(y, theta, xi) {
        rhorho1 <- tauh[length(tauh)]
        rhorho2 <- sum(tauh * proph)
        Zetah = qnorm(cumsum(proph), 0, 1)
        zetah = sqrt(xi^2 * theta[2]^2 * SX^2 + xi^2 * theta[3]^2 + 
            sigma^2) * Zetah + xi * theta[1] + xi * theta[2] * 
            EX
        for (h in 1:(length(tauh) - 1)) {
            rhorho1 <- rhorho1 + (tauh[h] - tauh[h + 1]) * pnorm((zetah[h] - 
                xi * (theta[1] + theta[2] * if (is.matrix(y)) {
                  y[, 1]
                }
                else {
                  y[1]
                }))/sqrt(sigma^2 + xi^2 * theta[3]^2))
        }
        return(rhorho1/rhorho2)
    }
    xihat = function(Obs) {
        (sum((Obs$z * Obs$y[, 2]/Obs$pik)))/(sum((Obs$y[, 2]^2/Obs$pik)))
    }
    xihatfunc1 <- function(y, z, pik) {
        cbind(z * y[, 2]/pik, y[, 2]^2/pik)
    }
    xihatfunc2 <- function(u) {
        u[1]/u[2]
    }
    thetaht = function(Obs) {
        lmm <- lm(Obs$y[, 2] ~ cbind(Obs$y[, 1]), weights = 1/Obs$pik)
        sigmahat <- sqrt(sum(lmm$residuals^2/Obs$pik)/sum(1/Obs$pik))
        return(c(as.vector(lmm$coefficients), sigmahat))
    }
    thetaniais = function(Obs) {
        lmm <- lm(y[, 2] ~ cbind(y[, 1]))
        return(c(as.vector(lmm$coefficients), summary(lmm)$sigma))
    }
    return(list(theta = theta, xi = xi, conditionalto = conditionalto, 
        rloiy = rloiy, ploi = function(y) {
            pnorm(y[, 2], mean = theta[1] + theta[2] * y[, 1], 
                sd = theta[3])
        }, dloi = dloi, dloitheta = dloitheta, rloiz = rloiz, 
        dloilim = function(y) {
            return(rho(y) * dnorm(y[, 2], theta[1] + theta[2] * 
                y[, 1], theta[3]))
        }, ploilim = function(y) {
        }, Scheme = Scheme, rho = rho, rhothetaxi = rhothetaxi, 
        rhoxthetaxi = rhoxthetaxi, vinf = function(y) {
            tau * rho(y) - (tau * rho(y))^2
        }, En = En, tau = tau, zetah = zetah, xihat = xihat, 
        xihatfunc1 = xihatfunc1, xihatfunc2 = xihatfunc2, xihatfuncdim = 2, 
        thetaht = thetaht, thetaniais = thetaniais, conditionalto = conditionalto, 
        supportY = c(-0.5, 5)))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
