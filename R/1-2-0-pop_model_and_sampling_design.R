#0. Rules

# A population model is a function of 

# - sampleparam,
# - theta
# - xi
# - param (may be anything)

# A population model returns a list with

# - theta
# - xi
# - param
# - dloiy : function of y that gives the pdf of Y in y
# - dloiytheta : function of y that gives the pdf of Y in y
# - ploi : cdf
# - ploilim
#  -rloiz - a function of y that returns a random vector (Z) of same size of y 
# - rloiy - a function of N the population size, that returns a random vector (Y)
# - rho - a function of y that implicitly depends on xi and theta
# - rhothetaxi - a function of y,theta,xi
# - Scheme - the sample method used. (a function of z)
# - vinf - function(y){sampleparam$tau*y},
# - En - a function of N that gives the expected value of the sample size
# - sampleparam - a list containing the sample parameters
# - logl1prime
# - I11 a function with no parameter that returns the first element of information matrix
# - I12
# - Sigma - the matrix Sigma of the paper
# - xihat=function(y,z,s) - an estimator of xi
# - thetaht=function(y,z,s) - an horvitz thompson  like estimator
# - thetahat=function(y,z,s) - the Mle estimator
# - thetaniais=function(y,z,s) - a naive estimator 
# - calculsintermediairespourjac - a function of (y) that will avoid to compute many times the same thing
#      when calculating the jacobian, the log likelihood, the hessian...
# - ll the log likelihood
# - derivethetaxi1
# - tau
# - V - the asymptotic varaiance of $\sqrt{n_\gamma \hat{theta}\gamma}$
# - VHT - the asymptotic variance of $\sqrt{n_\gamma \tilde{theta}\gamma}$
# - Vnaive - the asymptotic variance of \sqrt{n_\gamma}\times naive estimator



#
# Sample maximum likelihood estimation example.  Run the first part, up until the simulation,
# line by line to check the analytical computations against Monte Carlo values.   
#

#1. Some generic tools.


#2. The models


##1.5. Population and sample realisation
genere<-function(m=m,N=1000,Y=NA){
  Scheme=m$Scheme
  Yg<-m$rloiy(N);if(!is.na(Y)){Yg<-Y}
  Zg<-m$rloiz(Yg);
  pik<-Scheme$Pik(Zg);
  Sg<-Scheme$S(Zg);
  demarc<-Scheme$demarc(Zg);
  Ig<-rep(0,N);for(k in unique(Sg)){Ig[k]<-sum(Sg==k)}
  NHT=sum(1/pik[Sg])
  n=sum(Ig)
Gg=list(N=N,Yg=Yg,Zg=Zg,pik=pik,Sg=Sg,Ig=Ig,n=n,NHT=NHT,demarc=demarc)
return(Gg)
}
