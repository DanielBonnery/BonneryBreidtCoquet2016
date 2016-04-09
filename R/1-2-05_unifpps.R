model.Pareto.bernstrat<-function(sampleparam,theta,xi,param){
    #sampleparam is a list with tauh
  tauh<-sampleparam$tauh
  return(
  list(
   theta=theta,
    xi=xi,
     param=param,
    rloiy=function(N){exp(-log(1-runif(N))/theta)},
  ploi=function(y){(y>=1)*(1-(1/max(y,1)^theta))},
  ploilim=function(y){1-1/pgamma(y,3/2,2)},
  rloiz=function(y){rbinom(length(y),size=1,prob=1/y^xi)},
  dloi=function(y){theta/(y^(theta+1))},
  dloitheta=function(y,theta){theta/(y^(theta+1))},
  Scheme=StratBern(sampleparam),
  calculsintermediairespourjac=function(y){},
  Jacobiane=function(listeqtes,theta,xi,n){},
  hessiane=function(listeqtes,theta,xi,n){},
  rhothetaxi=function(y,theta,xi){(cbind(1-theta/(xi+theta),theta/(xi+theta))%*%t(sampleparam$tauh))^{-1}*(cbind(1/(y^xi), 1-1/(y^xi))%*%t(sampleparam$tauh)},
  rho=function(y){(cbind(1-theta/(xi+theta),theta/(xi+theta))%*%t(sampleparam$tauh))^{-1}*(cbind(1/(y^xi), 1-1/(y^xi))%*%t(sampleparam$tauh)},
  sampleparam=sampleparam,
  tau=tauh[1]+(tauh[2]-tauh[1])*theta/(theta+xi),
  #logl1prime=logl1prime,
  I11=-(-1/(theta+xi)^2+tauh[2]^2/(tauh[1]*xi+tauh[2]*theta)^2-1/theta^2),
  I12=-(-1/(theta+xi)^2+tauh[1]*tauh[2]/(tauh[1]*xi+tauh[2]*theta)^2),
  xihat=function(y,z,s){
    pik<-m$Scheme$Pik(z)[s]
    HT_y<-sum(y[s]/pik)
    HT_1<-sum(1/pik)
    HT_yz<-sum(y[s]*z[s]/pik)
    HT_theta<-(HT_y/(HT_y-HT_1))
    HT_xi<-HT_theta*((HT_1-HT_yz)/HT_yz)+1
    return(HT_xi)},
  thetaniais=function(y,z,s){mean(y)},
  thetaht=function(y,z,s){
    pik<-m$Scheme$Pik(z)[s]
    HT_y<-sum((y[s]/pik))
    HT_1<-sum(1/pik)
    HT_yz<-sum(y[s]*z[s]/pik)
    HT_theta<-(HT_y/(HT_y-HT_1))
    return(HT_theta)},
  thetahat=function(y,z,s){NULL},
  Sigma<-function(){
    Sigma<-matrix(0,2,2)
    # Check that Taylor deviates have mean zero, and compute their second moment.
    E_I<-tauh[1]+(tauh[2]-tauh[1])*theta/(theta+xi)
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
    #
    # Mean and variance of the Horvitz-Thompson plug-in estimator of xi
    Var_HT_xi<-E_taylor_dev_squared/N
    Sigma[2,2]<-N*E_I*Var_HT_xi 
    Var_Mean_Score<-(1/(E_I*N)^2)*N*(E_logY2I-E_logYI^2/E_I)
    Sigma[1,1]<-N*E_I*Var_Mean_Score
    return(Sigma)
  },
  supportY=c(-.1,2.1)))
  }