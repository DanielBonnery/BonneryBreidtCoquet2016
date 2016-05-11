\name{simulation.summary}
\alias{simulation.summary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Creates a table from simulation data}
\description{
This function creates a dataframe out of the output of the simulation_data function.
}
\usage{
simulation.summary(table_data)
}
\arguments{
  \item{table_data}{A list, output of simulation_data.}
}
\details{
Returns a dataframe that contains a summary (bias, variance, MSE, ... of different estimators (Sample, Pseudo, Naive and Full MLE))}
\value{
A data frame
}
\references{
}
\author{
Daniel Bonnery
}
\note{
}

\seealso{
}
\examples{
##---- Should be DIRECTLY executable !! ----
table_data<-Simulation_data(popmodelfunction = model.Pareto.bernstrat,
                                                    theta=4,
                                                    xi=xi,
                                                    conditionalto=list(N=80,sampleparam=list(tauh=c(0.5,0.7))),
                                                    nbreps=30)
simulation.summary(table_data)

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
        names(X) <- c("Estimator", "$\\theta$", "$\\xi$", paste0("Conditional to (", 
            paste(names(unlist(table_data[[1]]$model$conditionalto)), 
                collapse = ","), ")"), "Mean", "\% Relative Bias", 
            "RMSE Ratio", "Empirical Variance", "Asymptotic Variance")
        X
    })
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
