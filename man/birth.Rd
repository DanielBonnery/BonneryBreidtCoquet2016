\name{birth}
\alias{birth}
\docType{data}
\title{Birth weight and gestational age data, from 1988 NMIH survey}
\description{
Birth weight, gestational age sampling weight and stratum for 90 observations from
the 1988 National Maternal and Infant Health Survey.
}
\usage{data("birth")}
\format{
  A data frame with 90 observations on the following 4 variables.
  \describe{
    \item{\code{stratum}}{Stratum}
    \item{\code{weight}}{Sampling weight}
    \item{\code{birthwgt}}{Birth weight}
    \item{\code{GestAge}}{Gestational age}
  }
}
\details{
Contains data about  at-risk infants
from the 1988 National Maternal and Infant Health Survey (NMIHS), described in Korn
and Graubard (1999, Example 4.3-1). 
The design used birth certificates to oversample
low birthweight infants. Fuller (2009, Example 6.3.1) simulated a five-per-stratum de-
sign with 18 strata (90 observations) to mimic properties of NMIHS, and we used those
data to generate parameter values for our simulation. 

}
\source{
Korn, E. L. and Graubard, B. I. (1999). Analysis of Health Surveys. Wiley Series in
Probability and Mathematical Statistics. John Wiley & Sons Inc., New York
}
\references{
Fuller, W. A. (2009). Sampling Statistics. John Wiley & Sons Inc., New York. 560

Korn, E. L. and Graubard, B. I. (1999). Analysis of Health Surveys. Wiley Series in
Probability and Mathematical Statistics. John Wiley & Sons Inc., New York
}
\examples{
data(birth)
}
\keyword{datasets}
