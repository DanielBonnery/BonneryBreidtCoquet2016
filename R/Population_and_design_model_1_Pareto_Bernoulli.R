#2.1. Pareto distribution
model.Pareto.bernstrat<-function(theta=1,xi=1,conditionalto=list(N=1000,sampleparam=list(tauh=c(.1,.5)))){
    #sampleparam is a list with tauh
  suppressMessages(attach(conditionalto))
  suppressMessages(attach(sampleparam))
  
  calculeSigma<-function(){
    # Check that Taylor deviates have mean zero, and compute their second moment.
    E_I<-tauh[1]+(tauh[2]-tauh[1])*theta/(theta+xi)
    E_Z<-theta/(theta+xi)
    E_Y<-theta/(theta-1)
    E_logYI<-tauh[1]/theta+(tauh[2]-tauh[1])*theta/(theta+xi)^2
    E_logY2I<-2*tauh[1]/theta^2+(tauh[2]-tauh[1])*theta*2/(theta+xi)^3
    E_YZ<-theta/(theta+xi-1)
    E_Y2<-theta/(theta-2)
    E_Y2Z<-theta/(theta+xi-2)
    E_Y2_pi<-E_Y2/tauh[1]+(1/tauh[2]-1/tauh[1])*E_Y2Z
    E_YZ_pi<-theta/(tauh[2]*(theta+xi-1))
    E_Y2Z_pi<-E_Y2Z/tauh[2]
    E_Y_pi<-E_Y/tauh[1]+(1/tauh[2]-1/tauh[1])*E_YZ
    E_1_pi<-1/tauh[1]+(1/tauh[2]-1/tauh[1])*E_Z
    E_YZlogY<-theta/(theta+xi-1)^2
    E_YlogY<-theta/(theta-1)^2
    alpha_1<- (E_YZ-1)/(E_YZ*(E_Y-1)^2)
    alpha_2<- -E_Y/((E_Y-1)*E_YZ^2)
    alpha_3<- (E_Y/E_YZ)*(E_Y-E_YZ)/(E_Y-1)^2
    E_taylor_dev<-alpha_1*E_Y+alpha_2*E_YZ+alpha_3
    E_taylor_dev_squared<-alpha_1^2*E_Y2_pi+2*alpha_1*alpha_2*E_Y2Z_pi+2*alpha_1*alpha_3*E_Y_pi+alpha_2^2*E_Y2Z_pi+2*alpha_2*alpha_3*E_YZ_pi+alpha_3^2*E_1_pi
    # Mean and variance of the Horvitz-Thompson plug-in estimator of xi
    Var_HT_xi<-E_taylor_dev_squared
    Var_Mean_Score<-(1/(E_I)^2)*(E_logY2I-E_logYI^2/E_I)
    Sigma<-matrix(c(E_I*Var_Mean_Score,0,0,E_I*Var_HT_xi),2,2)
    return(Sigma)
  }
  tau<-tauh[1]+(tauh[2]-tauh[1])*theta/(theta+xi)
  Sigma<-calculeSigma()
  I11<--(((tauh[2]-tauh[1])*xi)/((theta+xi)*(tauh[2]*theta+tauh[1]*xi))*(1/(theta+xi)+tauh[2]/(tauh[2]*theta+tauh[1]*xi))-1/theta^2);
  I12<--((tauh[2]-tauh[1])*((tauh[1]*xi)/(tauh[2]*theta+tauh[1]*xi)+xi/(theta+xi)-1))/((theta+xi)*(tauh[2]*theta+tauh[1]*xi));
  I22=NA
  I=matrix(c(I11,I12,I12,I22),2,2)
  
  rloiy=function(){exp(-log(1-runif(conditionalto$N))/theta)}
  rloiz=function(y){rbinom(length(y),size=1,prob=1/y^xi)}
  
  
  return(
  list(
   theta=theta,
    xi=xi,
    conditionalto=conditionalto,
    rloiy=rloiy,
    ploi=function(y){pploi<-function(y){(y>=1)*(1-(1/max(y,1)^theta))}
                   return(sapply(y,pploi))},
  ploilim=function(y){1-1/pgamma(y,3/2,2)},
  rloiz=rloiz,
  dloi=function(y){theta/(y^(theta+1))},
  dloitheta=function(y,theta){theta/(y^(theta+1))},
  Scheme=StratBern(sampleparam),
  calculsintermediairespourjac=function(y){},
  Jacobiane=function(listeqtes,theta,xi,n){},
  hessiane=function(listeqtes,theta,xi,n){},
  rhoxthetaxi=function(x,theta,xi){1},
  rhothetaxi=function(y,theta,xi){as.vector(((sampleparam$tauh%*%rbind(1-theta/(xi+theta),theta/(xi+theta)))^{-1})[1,1]*
                                    (sampleparam$tauh%*%rbind(1-1/(as.vector(y)^xi), 1/(as.vector(y)^xi))))},
  rho=function(y){ as.vector(((sampleparam$tauh%*%rbind(xi/(xi+theta),theta/(xi+theta)))^{-1})[1,1]*
                                    (sampleparam$tauh%*%rbind(1-1/(as.vector(y)^xi),1/(as.vector(y)^xi))))},
  tau=tau,
  #logl1prime=logl1prime,
  xihat=function(Obs){
    HT_y<-sum(Obs$y/Obs$pik)
    HT_1<-sum(1/Obs$pik)
    HT_yz<-sum(Obs$y*Obs$z/Obs$pik)
    (HT_y/(HT_y-HT_1))*((HT_1-HT_yz)/HT_yz)+1},
  xihatfunc1=function(y,z,pik){cbind(1/pik,y/pik,y*z/pik)},
  xihatfunc2=function(u){(u[2]/(u[2]-u[1]))*((u[1]-u[3])/u[3])+1},
  xihatfuncdim=3,
  thetaniais=function(Obs){HT_theta<-mean(Obs$y)/(mean(Obs$y)-1)},
  thetaht=function(Obs){
    HT_y<-sum((Obs$y/Obs$pik))
    HT_1<-sum(1/Obs$pik)
    HT_y/(HT_y-HT_1)},
  supportY=c(-.1,2.1)))}