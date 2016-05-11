model.dep.strat2<-function(
  theta=c(beta0=.5,beta1=1,sigma=1),
  xi=2,
  conditionalto=list(N=1000,sigma=1,EX=1,SX=1,sampleparam=list(proph=c(.7,.3),tauh=c(1/70,2/15)))){
  #for simplicity of notation
  suppressMessages(attach(conditionalto))
  suppressMessages(attach(sampleparam))
  ## objects related to population generation
  rloiy <- function(.conditionalto=conditionalto){
    x=qnorm((1:.conditionalto$N)/(.conditionalto$N+1),.conditionalto$EX,.conditionalto$SX);
      cbind(x,rnorm(x,mean=theta[1]+theta[2]*x,sd=theta[3]))}
   rloiz=function(y){rnorm(y[,2],mean=xi*y[,2],sd=sigma)}
  ## objects related to sampling frame and sample
  Scheme<- StratS(sampleparam)
  tau   <-sum(proph*tauh);
  Zetah <- qnorm(cumsum(proph),0,1)
  zetah <- sqrt(xi^2*theta[2]^2*SX^2+ xi^2*theta[3]^2+sigma^2)*Zetah+xi*theta[1]+xi*theta[2]*EX
  En    <- function(N){tau*N}#Global sampling rate (will be returned)
  # objects related to population and sample distribution
  dloitheta=function(y,theta=conditionalto$theta){dnorm(as.matrix(y)[,2],mean=theta[1]+theta[2]*as.matrix(y)[,1],sd=theta[3])}
  #Population generation function
  #Computation of rho function (function of y,theta,xi)
  rhothetaxi=function(y,theta,xi){
      #Initialisation of numerator and deniminator
      rhorho1<-tauh[length(tauh)];
      rhorho2<-tauh[length(tauh)]; 
      #Computation of limits of strata
      Zetah=qnorm(cumsum(proph),0,1)
      zetah=sqrt(xi^2*theta[2]^2*SX^2+ xi^2*theta[3]^2+sigma^2)*Zetah+xi*theta[1]+xi*theta[2]*EX
      #Computation of numerator and deniminator
      for(h in 1:(length(tauh)-1)){
        rhorho1<-rhorho1+(tauh[h]-tauh[h+1])*pnorm((zetah[h]-xi*as.matrix(y)[,2]                    )/ sigma);
        rhorho2<-rhorho2+(tauh[h]-tauh[h+1])*pnorm((zetah[h]-xi*(theta[1]+theta[2]*as.matrix(y)[,1]))/sqrt(sigma^2+xi^2*theta[3]^2));}
      return(rhorho1/rhorho2)}
  #Computation of rho function (function of y)
  rho=function(y){return(rhothetaxi(y,theta,xi))}
  rhoxthetaxi=function(y,theta,xi){
      H<-length(tauh)
      #Computation of limits of strata
      Zetah=qnorm(cumsum(proph),0,1)
      zetah=sqrt(xi^2*theta[2]^2*SX^2+ xi^2*theta[3]^2+sigma^2)*Zetah+xi*theta[1]+xi*theta[2]*EX
      #for(h in 1:(H-1)){rhorho1<-rhorho1+(tauh[h]-tauh[h+1])*pnorm((zetah[h]-xi*(theta[1]+theta[2]*as.matrix(y)[,1])))}
      cumsum(c(tauh[H],sapply(1:(H-1),function(h){(tauh[h]-tauh[h+1])*pnorm((zetah[h]-xi*(theta[1]+theta[2]*as.matrix(y)[,1])))})))/
               sum(tauh*proph)}
  #Computation of rho function (function of y)
  # objects related to estimation
   xihat=function(Obs){(sum((Obs$z*Obs$y[,2]/Obs$pik)))/(sum((Obs$y[,2]^2/Obs$pik)))}
   xihatfunc1 <-function(y,z,pik){cbind(z*y[,2]/pik,y[,2]^2/pik)}
   xihatfunc2 <-function(u){u[1]/u[2]}
    thetaht=function(Obs){
        lmm<-lm(Obs$y[,2]~cbind(Obs$y[,1]),weights=1/Obs$pik);
        sigmahat<-sqrt(sum(lmm$residuals^2/Obs$pik)/sum(1/Obs$pik))
        return(c(as.vector(lmm$coefficients),sigmahat))}
    thetaniais=function(Obs){
        lmm<-lm(Obs$y[,2]~cbind(Obs$y[,1]));
        return(c(as.vector(lmm$coefficients),summary(lmm)$sigma))}
  # Final result
  return(list(
    theta=theta,xi=xi,conditionalto=conditionalto,
    rloiy=rloiy,
    ploi=function(y){pnorm(y[,2],mean=theta[1]+theta[2]*y[,1],sd=theta[3])},
    dloitheta=dloitheta,
    rloiz=rloiz,
    dloilim=function(y){return(rho(y)*dnorm(y[,2],theta[1]+theta[2]*y[,1],theta[3]))},
    Scheme=Scheme,
    rho=rho,
    rhothetaxi=rhothetaxi,
    rhoxthetaxi=rhoxthetaxi,
    vinf=function(y){tau*rho(y)-(tau*rho(y))^2},
    En=En,
    tau=tau,
    zetah=zetah,
    xihat=xihat,
    xihatfunc1=xihatfunc1,
    xihatfunc2=xihatfunc2,
    xihatfuncdim=2,
    thetaht=thetaht,
    thetaniais=thetaniais))}