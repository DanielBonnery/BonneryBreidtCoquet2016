model.dep.strat2<-function(sampleparam=list(proph=c(.7,.3),tauh=c(1/70,2/15)),
                           theta=c(.5,1,1),
                           xi=2,
                           param=list(sigma=1,EX=1,SX=1)){
  #for simplicity of notation
  proph<-sampleparam$proph
  tauh <-sampleparam$tauh
  sigma<-param$sigma
  EX   <-param$EX
  SX   <-param$SX

  ##__________________________________________________________________________
  ## objects related to population generation
  ##__________________________________________________________________________
    rloix <- function(N){qnorm((1:N)/(N+1),EX,SX)}
    rloiy.x <- function(x){
      return(rnorm(x,mean=theta[1]+theta[2]*x,sd=theta[3]))}
    rloiy <- function(N){
      x=rloix(N);
      y<-rloiy.x(x)
      return(cbind(x,y))}
   rloiz=function(y){rnorm(y[,2],mean=xi*y[,2],sd=sigma)}
  ##__________________________________________________________________________
  ## objects related to sampling frame and sample
  ##__________________________________________________________________________
  Scheme <- StratS(sampleparam)
  tau  <-sum(proph*tauh);
  Zetah <- qnorm(cumsum(proph),0,1)
  zetah <- sqrt(xi^2*theta[2]^2*SX^2+ xi^2*theta[3]^2+sigma^2)*Zetah+xi*theta[1]+xi*theta[2]*EX
  En <- function(N){tau*N}#Global sampling rate (will be returned)
  #__________________________________________________________________________
  # objects related to population and sample distribution
  #__________________________________________________________________________
  ##returns the pdf of y given x on the population
  ##returns the pdf of y given x on the population as a function of x,y and theta
  dloitheta=function(y,theta){
    if(is.matrix(y)){return(dnorm(y[,2],mean=theta[1]+theta[2]*y[,1],sd=theta[3]))} 
    if(is.vector(y)){return(dnorm(y[2],mean=theta[1]+theta[2]*y[1],sd=theta[3]))}}
  dloi=function(y){dloitheta(y,theta)}
  #Population generation function
  #Computation of rho function (function of y,theta,xi)
  rhothetaxi=function(y,theta,xi){
      if(is.matrix(y)){x<-y[,1];Y<-y[,2]} 
      if(is.vector(y)){x<-y[1];Y<-y[2]}
      y<-Y
      #Initialisation of numerator and deniminator
      rhorho1<-tauh[length(tauh)];
      rhorho2<-tauh[length(tauh)]; 
      #Computation of limits of strata
      Zetah=qnorm(cumsum(proph),0,1)
      zetah=sqrt(xi^2*theta[2]^2*SX^2+ xi^2*theta[3]^2+sigma^2)*Zetah+xi*theta[1]+xi*theta[2]*EX
      #Computation of numerator and deniminator
      for(h in 1:(length(tauh)-1)){
        rhorho1<-rhorho1+(tauh[h]-tauh[h+1])*pnorm((zetah[h]-xi*y                    )/ sigma);
        rhorho2<-rhorho2+(tauh[h]-tauh[h+1])*pnorm((zetah[h]-xi*(theta[1]+theta[2]*x))/sqrt(sigma^2+xi^2*theta[3]^2));}
      return(rhorho1/rhorho2)}
  #Computation of rho function (function of y)
  rho=function(y){return(rhothetaxi(y,theta,xi))}
  rhoxthetaxi=function(x,theta,xi){
      if(is.matrix(x)){x<-x[,1]}
      #Initialisation of numerator and deniminator
      rhorho1<-tauh[length(tauh)]; 
        rhorho2<-sum(tauh*proph);
      #Computation of limits of strata
      Zetah=qnorm(cumsum(proph),0,1)
      zetah=sqrt(xi^2*theta[2]^2*SX^2+ xi^2*theta[3]^2+sigma^2)*Zetah+xi*theta[1]+xi*theta[2]*EX
      #Computation of numerator and deniminator
      for(h in 1:(length(tauh)-1)){
        rhorho1<-rhorho1+(tauh[h]-tauh[h+1])*pnorm((zetah[h]-xi*(theta[1]+theta[2]*x))/sqrt(sigma^2+xi^2*theta[3]^2));}
      return(rhorho1/rhorho2)}
  #Computation of rho function (function of y)
  rho.x=function(x){return(rhoxthetaxi(x,theta,xi))}
  
  #__________________________________________________________________________
  # objects related to estimation
  #__________________________________________________________________________
   xihat=function(y,z,s){
      pik<-StratS(sampleparam)$Pik(z); #inclusion probabilities
      s.zy<-sum((z*y[,2]/pik)[s]) #HT estimator of $\sum_{k=1}^N Y_k Z_k$
      s.y2<-sum((y[,2]^2/pik)[s])  #HT estimator of $\sum_{k=1}^N Y_k^2$
      xi.hat<-(s.zy)/(s.y2)    #estimator of $xi$}
      return(xi.hat)}
   xihat=function(y,z,s){
      pik<-StratS(sampleparam)$Pik(z); #inclusion probabilities
      s.zy<-sum((z*y[,2]/pik)[s]) #HT estimator of $\sum_{k=1}^N Y_k Z_k$
      s.y2<-sum((y[,2]^2/pik)[s])  #HT estimator of $\sum_{k=1}^N Y_k^2$
      xi.hat<-(s.zy)/(s.y2)    #estimator of $xi$}
      return(xi.hat)}
   xihatfunc1 <-function(y,z,pik){c(z*y[,2]/pik,y[,2]^2/pik)}
   xihatfunc2 <-function(u){u[1]/u[2]}
  
    thetaht=function(y,z,s){
        piks<-StratS(sampleparam)$Pik(z)[s];
        lmm<-lm(y[s,2]~cbind(y[s,1]),weights=1/piks);
        sigmahat<-sqrt(sum(lmm$residuals^2/piks)/sum(1/piks))
        return(c(as.vector(lmm$coefficients),sigmahat))}
    thetaniais=function(y,z,s){
        lmm<-lm(y[s,2]~cbind(y[s,1]));
        return(c(as.vector(lmm$coefficients),summary(lmm)$sigma))}
    thetahat=function(y,z,s){}

  #__________________________________________________________________________
  # Final result
  #__________________________________________________________________________

        
  return(list(
    #returns the entries of the function
    sampleparam=sampleparam,theta=theta,xi=xi,param=param,
    #returns the function that will generate the population give a population size N
    rloix=rloix,
    rloiy=rloiy,
    rloiy.x=rloiy.x,
    #returns the cdf of y given x on the population
    ploi=function(y)           {pnorm(y[,2],mean=theta[1]+theta[2]*y[,1],sd=theta[3])},
    #returns the pdf of y given x on the population
    dloi=dloi,
    #returns the pdf of y given x on the population as a function of x,y and theta

    dloitheta=dloitheta,
    rloiz=rloiz,
    dloilim=function(y){return(rho(y)*dnorm(y[,2],theta[1]+theta[2]*y[,1],theta[3]))},
    ploilim=function(y){}, #no formal expression
    Scheme=Scheme,
    rho=rho,
    rhothetaxi=rhothetaxi,
    rho.x=rho.x,
    rhoxthetaxi=rhoxthetaxi,
    vinf=function(y){tau*rho(y)-(tau*rho(y))^2},
    En=En,
    tau=tau,
    zetah=zetah,
    xihat=xihat,
    xihatfunc1 =xihatfunc1,
    xihatfunc2 = xihatfunc2,
    xihatfuncdim = 2,
    thetaht=thetaht,
    thetaniais=thetaniais,
    thetahat=thetahat,
    supportY=c(-.5,5)))}
