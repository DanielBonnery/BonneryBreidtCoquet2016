\name{model.Pareto.bernstrat}
\alias{model.Pareto.bernstrat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
model.Pareto.bernstrat(theta = 1, xi = 1, conditionalto = list(N = 1000, sampleparam = list(tauh = c(0.1, 0.5))))
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
function (theta = 1, xi = 1, conditionalto = list(N = 1000, sampleparam = list(tauh = c(0.1, 
    0.5)))) 
{
    attach(conditionalto)
    attach(sampleparam)
    calculeSigma <- function() {
        E_I <- tauh[1] + (tauh[2] - tauh[1]) * theta/(theta + 
            xi)
        E_Z <- theta/(theta + xi)
        E_Y <- theta/(theta - 1)
        E_logYI <- tauh[1]/theta + (tauh[2] - tauh[1]) * theta/(theta + 
            xi)^2
        E_logY2I <- 2 * tauh[1]/theta^2 + (tauh[2] - tauh[1]) * 
            theta * 2/(theta + xi)^3
        E_YZ <- theta/(theta + xi - 1)
        E_Y2 <- theta/(theta - 2)
        E_Y2Z <- theta/(theta + xi - 2)
        E_Y2_pi <- E_Y2/tauh[1] + (1/tauh[2] - 1/tauh[1]) * E_Y2Z
        E_YZ_pi <- theta/(tauh[2] * (theta + xi - 1))
        E_Y2Z_pi <- E_Y2Z/tauh[2]
        E_Y_pi <- E_Y/tauh[1] + (1/tauh[2] - 1/tauh[1]) * E_YZ
        E_1_pi <- 1/tauh[1] + (1/tauh[2] - 1/tauh[1]) * E_Z
        E_YZlogY <- theta/(theta + xi - 1)^2
        E_YlogY <- theta/(theta - 1)^2
        alpha_1 <- (E_YZ - 1)/(E_YZ * (E_Y - 1)^2)
        alpha_2 <- -E_Y/((E_Y - 1) * E_YZ^2)
        alpha_3 <- (E_Y/E_YZ) * (E_Y - E_YZ)/(E_Y - 1)^2
        E_taylor_dev <- alpha_1 * E_Y + alpha_2 * E_YZ + alpha_3
        E_taylor_dev_squared <- alpha_1^2 * E_Y2_pi + 2 * alpha_1 * 
            alpha_2 * E_Y2Z_pi + 2 * alpha_1 * alpha_3 * E_Y_pi + 
            alpha_2^2 * E_Y2Z_pi + 2 * alpha_2 * alpha_3 * E_YZ_pi + 
            alpha_3^2 * E_1_pi
        Var_HT_xi <- E_taylor_dev_squared
        Var_Mean_Score <- (1/(E_I)^2) * (E_logY2I - E_logYI^2/E_I)
        Sigma <- matrix(c(E_I * Var_Mean_Score, 0, 0, E_I * Var_HT_xi), 
            2, 2)
        return(Sigma)
    }
    tau <- tauh[1] + (tauh[2] - tauh[1]) * theta/(theta + xi)
    Sigma <- calculeSigma()
    I11formula <- -(((tauh[2] - tauh[1]) * xi)/((theta + xi) * 
        (tauh[2] * theta + tauh[1] * xi)) * (1/(theta + xi) + 
        tauh[2]/(tauh[2] * theta + tauh[1] * xi)) - 1/theta^2)
    I12formula <- -((tauh[2] - tauh[1]) * ((tauh[1] * xi)/(tauh[2] * 
        theta + tauh[1] * xi) + xi/(theta + xi) - 1))/((theta + 
        xi) * (tauh[2] * theta + tauh[1] * xi))
    rloiy = function(N, conditionalto = NULL) {
        exp(-log(1 - runif(N))/theta)
    }
    rloiz = function(y) {
        rbinom(length(y), size = 1, prob = 1/y^xi)
    }
    return(list(theta = theta, xi = xi, conditionalto = conditionalto, 
        rloiy = rloiy, ploi = function(y) {
            pploi <- function(y) {
                (y >= 1) * (1 - (1/max(y, 1)^theta))
            }
            return(sapply(y, pploi))
        }, ploilim = function(y) {
            1 - 1/pgamma(y, 3/2, 2)
        }, rloiz = rloiz, dloi = function(y) {
            theta/(y^(theta + 1))
        }, dloitheta = function(y, theta) {
            theta/(y^(theta + 1))
        }, Scheme = StratBern(sampleparam), calculsintermediairespourjac = function(y) {
        }, Jacobiane = function(listeqtes, theta, xi, n) {
        }, hessiane = function(listeqtes, theta, xi, n) {
        }, rhoxthetaxi = function(x, theta, xi) {
            1
        }, rhothetaxi = function(y, theta, xi) {
            as.vector(((sampleparam$tauh \%*\% rbind(1 - theta/(xi + 
                theta), theta/(xi + theta)))^{
                -1
            })[1, 1] * (sampleparam$tauh \%*\% rbind(1 - 1/(as.vector(y)^xi), 
                1/(as.vector(y)^xi))))
        }, rho = function(y) {
            as.vector(((sampleparam$tauh \%*\% rbind(xi/(xi + theta), 
                theta/(xi + theta)))^{
                -1
            })[1, 1] * (sampleparam$tauh \%*\% rbind(1 - 1/(as.vector(y)^xi), 
                1/(as.vector(y)^xi))))
        }, tau = tau, I11MC = function() {
        }, I12MC = function() {
        }, I11formula = I11formula, I12formula = I12formula, 
        I11 = I11formula, I12 = I12formula, xihat = function(Obs) {
            HT_y <- sum(Obs$y/Obs$pik)
            HT_1 <- sum(1/Obs$pik)
            HT_yz <- sum(Obs$y * Obs$z/Obs$pik)
            (HT_y/(HT_y - HT_1)) * ((HT_1 - HT_yz)/HT_yz) + 1
        }, xihatfunc1 = function(y, z, pik) {
            cbind(1/pik, y/pik, y * z/pik)
        }, xihatfunc2 = function(u) {
            (u[2]/(u[2] - u[1])) * ((u[1] - u[3])/u[3]) + 1
        }, xihatfuncdim = 3, thetaniais = function(Obs) {
            HT_theta <- mean(Obs$y)/(mean(Obs$y) - 1)
        }, thetaht = function(Obs) {
            HT_y <- sum((Obs$y/pik))
            HT_1 <- sum(1/Obs$pik)
            HT_y/(HT_y - HT_1)
        }, Sigma = calculeSigma(), V = Sigma[1, 1]/I11formula^2 + 
            I12formula/I11formula^2 * (Sigma[2, 2] * I12formula - 
                2 * Sigma[1, 2]), VHT = tau * (((tauh[1]^(-1) - 
            1)^2 * tauh[1] + (1 - tauh[1])) * (-(2 - theta)^(-1) * 
            theta + (2 - xi - theta)^(-1) * theta) + (tauh[2]^(-1) - 
            1)^2 * tauh[2] + (1 - tauh[2]) * (xi + theta - 2)^(-1) * 
            theta + (-(2 - theta)^(-1) * theta - (1 - theta)^(-2) * 
            theta^(2))) * (theta - 1)^(4), Vniais = NULL, supportY = c(-0.1, 
            2.1)))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
