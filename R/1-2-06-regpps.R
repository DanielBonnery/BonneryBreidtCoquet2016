##2.3 regression model and pps
#sampleparam<-list(tau=.1)
#theta=c(2,2,1)
#xi=4
#param=list()

model.reg.pps<-function(sampleparam,theta,xi,param){
  tau<-sampleparam$tau
  sigma<-param$sigma
  
  ##5.3. Calculus of differentiate(Delta)
    deriveetheta1<-function(y,sampleparam,theta,xi,param){0}
    deriveetheta<-function(y,sampleparam,theta,xi,param){
          return(sapply(y,deriveetheta1,sampleparam=sampleparam,theta=theta,xi=xi,param=param))}
    deriveexi1<-function(y,sampleparam,theta,xi,param){0}
    deriveexi<-function(y,sampleparam,theta,xi,param){
          return(sapply(y,deriveexi1,sampleparam=sampleparam,theta=theta,xi=xi,param=param))}
    rhothetaxi=function(y,theta,xi){
    if(is.vector(y)){rh<-(y[2]*(y[2]>xi))/
                         (theta[1]+theta[2]*y[1])*(1-pnorm((xi-theta[1]-theta[2]*y[1])))+theta[3]*dnorm((xi-theta[1]-theta[2]*y[1])/theta[3])}
    if(is.matrix(y)){rh<-(y[,2]*(y[,2]>xi))/
                         (theta[1]+theta[2]*y[,1])*(1-pnorm((xi-theta[1]-theta[2]*y[,1])))+theta[3]*dnorm((xi-theta[1]-theta[2]*y[,1])/theta[3])}
    return(rh)};
    rho=function(y){rhothetaxi(y,xi,theta)};   
    cav<-function(sampleparam,theta,xi,param,nrep){
        y=0
        dd= deriveetheta(y,sampleparam=sampleparam,theta=theta,xi=xi,param=param)
        ddxi= deriveexi(y,sampleparam=sampleparam,theta=theta,xi=xi,param=param)
        rhorho=rhothetaxi(y,theta,xi)
        I11=mean(dd^2*rhorho)
        I12=mean(dd*ddxi*rhorho)
        tau<-sampleparam$tau;
         return(list(Sigma=1,Sigmah=1,MA=1,I11=1,I12=1,VHT=0,V=0,V1=0,Vniais=0))}
    cave=cav(sampleparam,theta,xi,param,10000);
  return(list(
    sampleparam=sampleparam,
    theta=theta,xi=xi,param=param,
    rloiy=function(N){
      x=5+qnorm(.025+.95*(1:N)/N);
      y<-theta[1]+theta[2]*x+theta[3]*rnorm(N)
      #plot(x,y);points(x[y>14],y[y>14],col='red')
      return(cbind(x,y))},
    ploi=function(y){pnorm(y[,2],theta[1]+theta[2]*y[,1],theta[3])},
    dloi=function(y){dnorm(y[,2],theta[1]+theta[2]*y[,1],theta[3])},
    dloitheta=function(y,theta){dnorm(y[,2],theta[1]+theta[2]*y[,1],theta[3])},
    rloiz=function(y){y[,2]*(y[,2]>xi)},
    dloilim=function(y){return(rho(y)*dnorm(y[,2],theta[1]+theta[2]*y[,1],theta[3]))},
    deriveetheta=deriveetheta,
    ploilim=function(y){
      yy<-rnorm(1000,theta,1);
      eps<-rnorm(10000,0,sigma);
      toto<-0
      for(h in 1:length(tauh)){
        limites<-sqrt(xi^2+sigma^2)*qnorm(cumsum(c(0,proph)))+xi*theta
        toto<-toto+proph[h]*tauh[h]*ecdf(yy[xi*yy+eps<=limites[h+1]&xi*yy+eps>limites[h]])(y)
        }
      return(toto/(sum(proph*tauh)))}, #no formal expression
    Scheme=SystematicPPS(sampleparam),
    rho=rho,
    rhothetaxi=rhothetaxi,
    vinf=function(y){tau*rho(y)-(tau*rho(y))^2},
  calculsintermediairespourjac=function(y){return(list(
    Sx=sum(y[,1]),
    Sx2=sum(y[,1]^2),
    Syx=sum(y[,1]*y[,2]),
    Sy=sum(y[,2]),
    Sy2=sum(y[,2]^2),
    y=y))},    
  Jacobiane=function(listeqtes,xi,theta,n){
    Sx<-listeqtes$Sx;
    Sy<-listeqtes$Sy;
    Sx2<-listeqtes$Sx2;
    Sy2<-listeqtes$Sy2;
    Syx<-listeqtes$Syx;
    Sxy<-listeqtes$Syx;
    x<-listeqtes$y[,1];
    y<-listeqtes$y[,2];
    a<-theta[1];b<-theta[2];sigma<-theta[3];
    Jlr<-
  cbind(-(xi*exp(-xi^2/(2*sigma^2)+(b*x*xi)/sigma^2+(a*xi)/sigma^2-(b^2*x^2)/(2*sigma^2)-(a*b*x)/sigma^2-a^2/(2*sigma^2)))/(-(sqrt(pi)*sigma*b*x*erf(xi/(sqrt(2)*sigma)-(b*x)/(sqrt(2)*sigma)-a/(sqrt(2)*sigma)))/sqrt(2)-(sqrt(pi)*sigma*a*erf(xi/(sqrt(2)*sigma)-(b*x)/(sqrt(2)*sigma)-a/(sqrt(2)*sigma)))/sqrt(2)+sigma^2*exp(-xi^2/(2*sigma^2)+(b*x*xi)/sigma^2+(a*xi)/sigma^2-(b^2*x^2)/(2*sigma^2)-(a*b*x)/sigma^2-a^2/(2*sigma^2))+(sqrt(pi)*sigma*b*x)/sqrt(2)+(sqrt(pi)*sigma*a)/sqrt(2))+erf(xi/(sqrt(2)*sigma)-(b*x)/(sqrt(2)*sigma)-a/(sqrt(2)*sigma))/(-b*x*erf(xi/(sqrt(2)*sigma)-(b*x)/(sqrt(2)*sigma)-a/(sqrt(2)*sigma))-a*erf(xi/(sqrt(2)*sigma)-(b*x)/(sqrt(2)*sigma)-a/(sqrt(2)*sigma))+(sqrt(2)*sigma*exp(-xi^2/(2*sigma^2)+(b*x*xi)/sigma^2+(a*xi)/sigma^2-(b^2*x^2)/(2*sigma^2)-(a*b*x)/sigma^2-a^2/(2*sigma^2)))/sqrt(pi)+b*x+a)-1/(-b*x*erf(xi/(sqrt(2)*sigma)-(b*x)/(sqrt(2)*sigma)-a/(sqrt(2)*sigma))-a*erf(xi/(sqrt(2)*sigma)-(b*x)/(sqrt(2)*sigma)-a/(sqrt(2)*sigma))+(sqrt(2)*sigma*exp(-xi^2/(2*sigma^2)+(b*x*xi)/sigma^2+(a*xi)/sigma^2-(b^2*x^2)/(2*sigma^2)-(a*b*x)/sigma^2-a^2/(2*sigma^2)))/sqrt(pi)+b*x+a),
    -(x*xi*exp(-xi^2/(2*sigma^2)+(b*x*xi)/sigma^2+(a*xi)/sigma^2-(b^2*x^2)/(2*sigma^2)-(a*b*x)/sigma^2-a^2/(2*sigma^2)))/(-(sqrt(pi)*sigma*b*x*erf(xi/(sqrt(2)*sigma)-(b*x)/(sqrt(2)*sigma)-a/(sqrt(2)*sigma)))/sqrt(2)-(sqrt(pi)*sigma*a*erf(xi/(sqrt(2)*sigma)-(b*x)/(sqrt(2)*sigma)-a/(sqrt(2)*sigma)))/sqrt(2)+sigma^2*exp(-xi^2/(2*sigma^2)+(b*x*xi)/sigma^2+(a*xi)/sigma^2-(b^2*x^2)/(2*sigma^2)-(a*b*x)/sigma^2-a^2/(2*sigma^2))+(sqrt(pi)*sigma*b*x)/sqrt(2)+(sqrt(pi)*sigma*a)/sqrt(2))+(x*erf(xi/(sqrt(2)*sigma)-(b*x)/(sqrt(2)*sigma)-a/(sqrt(2)*sigma)))/(-b*x*erf(xi/(sqrt(2)*sigma)-(b*x)/(sqrt(2)*sigma)-a/(sqrt(2)*sigma))-a*erf(xi/(sqrt(2)*sigma)-(b*x)/(sqrt(2)*sigma)-a/(sqrt(2)*sigma))+(sqrt(2)*sigma*exp(-xi^2/(2*sigma^2)+(b*x*xi)/sigma^2+(a*xi)/sigma^2-(b^2*x^2)/(2*sigma^2)-(a*b*x)/sigma^2-a^2/(2*sigma^2)))/sqrt(pi)+b*x+a)-x/(-b*x*erf(xi/(sqrt(2)*sigma)-(b*x)/(sqrt(2)*sigma)-a/(sqrt(2)*sigma))-a*erf(xi/(sqrt(2)*sigma)-(b*x)/(sqrt(2)*sigma)-a/(sqrt(2)*sigma))+(sqrt(2)*sigma*exp(-xi^2/(2*sigma^2)+(b*x*xi)/sigma^2+(a*xi)/sigma^2-(b^2*x^2)/(2*sigma^2)-(a*b*x)/sigma^2-a^2/(2*sigma^2)))/sqrt(pi)+b*x+a),
    -(xi^2*exp(-xi^2/(2*sigma^2)+(b*x*xi)/sigma^2+(a*xi)/sigma^2-(b^2*x^2)/(2*sigma^2)-(a*b*x)/sigma^2-a^2/(2*sigma^2)))/(-(sqrt(pi)*sigma^2*b*x*erf(xi/(sqrt(2)*sigma)-(b*x)/(sqrt(2)*sigma)-a/(sqrt(2)*sigma)))/sqrt(2)-(sqrt(pi)*sigma^2*a*erf(xi/(sqrt(2)*sigma)-(b*x)/(sqrt(2)*sigma)-a/(sqrt(2)*sigma)))/sqrt(2)+sigma^3*exp(-xi^2/(2*sigma^2)+(b*x*xi)/sigma^2+(a*xi)/sigma^2-(b^2*x^2)/(2*sigma^2)-(a*b*x)/sigma^2-a^2/(2*sigma^2))+(sqrt(pi)*sigma^2*b*x)/sqrt(2)+(sqrt(pi)*sigma^2*a)/sqrt(2))-(b*x*xi*exp(-xi^2/(2*sigma^2)+(b*x*xi)/sigma^2+(a*xi)/sigma^2-(b^2*x^2)/(2*sigma^2)-(a*b*x)/sigma^2-a^2/(2*sigma^2)))/(-(sqrt(pi)*sigma^2*b*x*erf(xi/(sqrt(2)*sigma)-(b*x)/(sqrt(2)*sigma)-a/(sqrt(2)*sigma)))/sqrt(2)-(sqrt(pi)*sigma^2*a*erf(xi/(sqrt(2)*sigma)-(b*x)/(sqrt(2)*sigma)-a/(sqrt(2)*sigma)))/sqrt(2)+sigma^3*exp(-xi^2/(2*sigma^2)+(b*x*xi)/sigma^2+(a*xi)/sigma^2-(b^2*x^2)/(2*sigma^2)-(a*b*x)/sigma^2-a^2/(2*sigma^2))+(sqrt(pi)*sigma^2*b*x)/sqrt(2)+(sqrt(pi)*sigma^2*a)/sqrt(2))-(a*xi*exp(-xi^2/(2*sigma^2)+(b*x*xi)/sigma^2+(a*xi)/sigma^2-(b^2*x^2)/(2*sigma^2)-(a*b*x)/sigma^2-a^2/(2*sigma^2)))/(-(sqrt(pi)*sigma^2*b*x*erf(xi/(sqrt(2)*sigma)-(b*x)/(sqrt(2)*sigma)-a/(sqrt(2)*sigma)))/sqrt(2)-(sqrt(pi)*sigma^2*a*erf(xi/(sqrt(2)*sigma)-(b*x)/(sqrt(2)*sigma)-a/(sqrt(2)*sigma)))/sqrt(2)+sigma^3*exp(-xi^2/(2*sigma^2)+(b*x*xi)/sigma^2+(a*xi)/sigma^2-(b^2*x^2)/(2*sigma^2)-(a*b*x)/sigma^2-a^2/(2*sigma^2))+(sqrt(pi)*sigma^2*b*x)/sqrt(2)+(sqrt(pi)*sigma^2*a)/sqrt(2))+(sqrt(2)*b*x*xi*exp(-xi^2/(2*sigma^2)+(b*x*xi)/sigma^2+(a*xi)/sigma^2-(b^2*x^2)/(2*sigma^2)-(a*b*x)/sigma^2-a^2/(2*sigma^2)))/(-(sqrt(pi)*sigma^2*b*x*erf(xi/(sqrt(2)*sigma)-(b*x)/(sqrt(2)*sigma)-a/(sqrt(2)*sigma)))/2-(sqrt(pi)*sigma^2*a*erf(xi/(sqrt(2)*sigma)-(b*x)/(sqrt(2)*sigma)-a/(sqrt(2)*sigma)))/2+(sigma^3*exp(-xi^2/(2*sigma^2)+(b*x*xi)/sigma^2+(a*xi)/sigma^2-(b^2*x^2)/(2*sigma^2)-(a*b*x)/sigma^2-a^2/(2*sigma^2)))/sqrt(2)+(sqrt(pi)*sigma^2*b*x)/2+(sqrt(pi)*sigma^2*a)/2)+(sqrt(2)*a*xi*exp(-xi^2/(2*sigma^2)+(b*x*xi)/sigma^2+(a*xi)/sigma^2-(b^2*x^2)/(2*sigma^2)-(a*b*x)/sigma^2-a^2/(2*sigma^2)))/(-(sqrt(pi)*sigma^2*b*x*erf(xi/(sqrt(2)*sigma)-(b*x)/(sqrt(2)*sigma)-a/(sqrt(2)*sigma)))/2-(sqrt(pi)*sigma^2*a*erf(xi/(sqrt(2)*sigma)-(b*x)/(sqrt(2)*sigma)-a/(sqrt(2)*sigma)))/2+(sigma^3*exp(-xi^2/(2*sigma^2)+(b*x*xi)/sigma^2+(a*xi)/sigma^2-(b^2*x^2)/(2*sigma^2)-(a*b*x)/sigma^2-a^2/(2*sigma^2)))/sqrt(2)+(sqrt(pi)*sigma^2*b*x)/2+(sqrt(pi)*sigma^2*a)/2)-exp(-xi^2/(2*sigma^2)+(b*x*xi)/sigma^2+(a*xi)/sigma^2-(b^2*x^2)/(2*sigma^2)-(a*b*x)/sigma^2-a^2/(2*sigma^2))/(-(sqrt(pi)*b*x*erf(xi/(sqrt(2)*sigma)-(b*x)/(sqrt(2)*sigma)-a/(sqrt(2)*sigma)))/sqrt(2)-(sqrt(pi)*a*erf(xi/(sqrt(2)*sigma)-(b*x)/(sqrt(2)*sigma)-a/(sqrt(2)*sigma)))/sqrt(2)+sigma*exp(-xi^2/(2*sigma^2)+(b*x*xi)/sigma^2+(a*xi)/sigma^2-(b^2*x^2)/(2*sigma^2)-(a*b*x)/sigma^2-a^2/(2*sigma^2))+(sqrt(pi)*b*x)/sqrt(2)+(sqrt(pi)*a)/sqrt(2)))
    Jlr<-apply(Jlr,2,sum);
    Jlf<-c(Sy/sigma^2-(b*Sx)/sigma^2-a/sigma^2,
           Sxy/sigma^2-(a*Sx)/sigma^2-(Sx2*b)/sigma^2,
           -(2*a*Sy)/sigma^3-(2*b*Sxy)/sigma^3+(2*a*b*Sx)/sigma^3+(Sx2*b^2)/sigma^3+a^2/sigma^3+Sy2/sigma^3)
return(Jlf+Jlr)},
  hessiane=function(listeqtes,xi,theta,n){
    Sx<-listeqtes$Sx;
    Sy<-listeqtes$Sy;
    Sx2<-listeqtes$Sx2;
    Sy2<-listeqtes$Sy2;
    Syx<-listeqtes$Syx;
    Sxy<-listeqtes$Syx;
    x<-listeqtes$y[,1];
    y<-listeqtes$y[,2];
    a<-theta[1];b<-theta[2];sigma<-theta[3];
    #computation of the hessian of log(f)
    Hlf<-matrix(NA,3,3);
    Hlf[1,1]<--1/sigma^2
    Hlf[1,2]<--Sx/sigma^2
    Hlf[1,3]<--(2*Sy)/sigma^3+(2*b*Sx)/sigma^3+(2*a)/sigma^3
    Hlf[2,2]<--Sx2/sigma^2
    Hlf[2,3]<--(2*Sxy)/sigma^3+(2*a*Sx)/sigma^3+(2*Sx2*b)/sigma^3
    Hlf[3,3]<-(6*a*Sy)/sigma^4+(6*b*Sxy)/sigma^4-(6*a*b*Sx)/sigma^4-(3*Sx2*b^2)/sigma^4-(3*a^2)/sigma^4-(3*Sy2)/sigma^4
    Hlf[2,1]<-Hlf[1,2];Hlf[3,2]<-Hlf[2,3];Hlf[3,1]<-Hlf[1,3];
    #computation of the hessian of log(rho)
    Hlr11<-(-erf((xi-b*x-a)/(sqrt(2)*sigma))/2+((xi-b*x-a)*exp(-(xi-b*x-a)^2/(2*sigma^2)))/(sqrt(2)*sqrt(pi)*sigma)+((b*x+a)*exp(-(xi-b*x-a)^2/(2*sigma^2)))/(sqrt(2)*sqrt(pi)*sigma)+1/2)^2/((b*x+a)*(1/2-erf((xi-b*x-a)/(sqrt(2)*sigma))/2)+(sigma*exp(-(xi-b*x-a)^2/(2*sigma^2)))/(sqrt(2)*sqrt(pi)))^2-
(((xi-b*x-a)^2*exp(-(xi-b*x-a)^2/(2*sigma^2)))/(sqrt(2)*sqrt(pi)*sigma^3)+((b*x+a)*(xi-b*x-a)*exp(-(xi-b*x-a)^2/(2*sigma^2)))/(sqrt(2)*sqrt(pi)*sigma^3)+exp(-(xi-b*x-a)^2/(2*sigma^2))/(sqrt(2)*sqrt(pi)*sigma))/((b*x+a)*(1/2-erf((xi-b*x-a)/(sqrt(2)*sigma))/2)+(sigma*exp(-(xi-b*x-a)^2/(2*sigma^2)))/(sqrt(2)*sqrt(pi)))
Hlr12<- ((x*(1/2-erf((xi-b*x-a)/(sqrt(2)*sigma))/2)+(x*(xi-b*x-a)*exp(-(xi-b*x-a)^2/(2*sigma^2)))/(sqrt(2)*sqrt(pi)*sigma)+(x*(b*x+a)*exp(-(xi-b*x-a)^2/(2*sigma^2)))/(sqrt(2)*sqrt(pi)*sigma))*
(-erf((xi-b*x-a)/(sqrt(2)*sigma))/2+((xi-b*x-a)*exp(-(xi-b*x-a)^2/(2*sigma^2)))/(sqrt(2)*sqrt(pi)*sigma)+((b*x+a)*exp(-(xi-b*x-a)^2/(2*sigma^2)))/(sqrt(2)*sqrt(pi)*sigma)+1/2))/(((b*x+a)*(1/2-erf((xi-b*x-a)/(sqrt(2)*sigma))/2)+(sigma*exp(-(xi-b*x-a)^2/(2*sigma^2)))/(sqrt(2)*sqrt(pi)))^2)-
((x*(xi-b*x-a)^2*exp(-(xi-b*x-a)^2/(2*sigma^2)))/(sqrt(2)*sqrt(pi)*sigma^3)+(x*(b*x+a)*(xi-b*x-a)*exp(-(xi-b*x-a)^2/(2*sigma^2)))/(sqrt(2)*sqrt(pi)*sigma^3)+(x*exp(-(xi-b*x-a)^2/(2*sigma^2)))/(sqrt(2)*sqrt(pi)*sigma))/((b*x+a)*(1/2-erf((xi-b*x-a)/(sqrt(2)*sigma))/2)+(sigma*exp(-(xi-b*x-a)^2/(2*sigma^2)))/(sqrt(2)*sqrt(pi)))
Hlr13<-  ((((xi-b*x-a)^2*exp(-(xi-b*x-a)^2/(2*sigma^2)))/(sqrt(2)*sqrt(pi)*sigma^2)+((b*x+a)*(xi-b*x-a)*exp(-(xi-b*x-a)^2/(2*sigma^2)))/(sqrt(2)*sqrt(pi)*sigma^2)+exp(-(xi-b*x-a)^2/(2*sigma^2))/(sqrt(2)*sqrt(pi)))*
(-erf((xi-b*x-a)/(sqrt(2)*sigma))/2+((xi-b*x-a)*exp(-(xi-b*x-a)^2/(2*sigma^2)))/(sqrt(2)*sqrt(pi)*sigma)+((b*x+a)*exp(-(xi-b*x-a)^2/(2*sigma^2)))/(sqrt(2)*sqrt(pi)*sigma)+1/2))/(((b*x+a)*(1/2-erf((xi-b*x-a)/(sqrt(2)*sigma))/2)+(sigma*exp(-(xi-b*x-a)^2/(2*sigma^2)))/(sqrt(2)*sqrt(pi)))^2)-
(((xi-b*x-a)^3*exp(-(xi-b*x-a)^2/(2*sigma^2)))/(sqrt(2)*sqrt(pi)*sigma^4)+((b*x+a)*(xi-b*x-a)^2*exp(-(xi-b*x-a)^2/(2*sigma^2)))/(sqrt(2)*sqrt(pi)*sigma^4)-((b*x+a)*exp(-(xi-b*x-a)^2/(2*sigma^2)))/(sqrt(2)*sqrt(pi)*sigma^2))/((b*x+a)*(1/2-erf((xi-b*x-a)/(sqrt(2)*sigma))/2)+(sigma*exp(-(xi-b*x-a)^2/(2*sigma^2)))/(sqrt(2)*sqrt(pi)))
Hlr22<-  (x*(1/2-erf((xi-b*x-a)/(sqrt(2)*sigma))/2)+(x*(xi-b*x-a)*exp(-(xi-b*x-a)^2/(2*sigma^2)))/(sqrt(2)*sqrt(pi)*sigma)+(x*(b*x+a)*exp(-(xi-b*x-a)^2/(2*sigma^2)))/(sqrt(2)*sqrt(pi)*sigma))^2/((b*x+a)*(1/2-erf((xi-b*x-a)/(sqrt(2)*sigma))/2)+(sigma*exp(-(xi-b*x-a)^2/(2*sigma^2)))/(sqrt(2)*sqrt(pi)))^2-
((x^2*(xi-b*x-a)^2*exp(-(xi-b*x-a)^2/(2*sigma^2)))/(sqrt(2)*sqrt(pi)*sigma^3)+(x^2*(b*x+a)*(xi-b*x-a)*exp(-(xi-b*x-a)^2/(2*sigma^2)))/(sqrt(2)*sqrt(pi)*sigma^3)+(x^2*exp(-(xi-b*x-a)^2/(2*sigma^2)))/(sqrt(2)*sqrt(pi)*sigma))/((b*x+a)*(1/2-erf((xi-b*x-a)/(sqrt(2)*sigma))/2)+(sigma*exp(-(xi-b*x-a)^2/(2*sigma^2)))/(sqrt(2)*sqrt(pi)))
Hlr23<- ((((xi-b*x-a)^2*exp(-(xi-b*x-a)^2/(2*sigma^2)))/(sqrt(2)*sqrt(pi)*sigma^2)+((b*x+a)*(xi-b*x-a)*exp(-(xi-b*x-a)^2/(2*sigma^2)))/(sqrt(2)*sqrt(pi)*sigma^2)+exp(-(xi-b*x-a)^2/(2*sigma^2))/(sqrt(2)*sqrt(pi)))*
(x*(1/2-erf((xi-b*x-a)/(sqrt(2)*sigma))/2)+(x*(xi-b*x-a)*exp(-(xi-b*x-a)^2/(2*sigma^2)))/(sqrt(2)*sqrt(pi)*sigma)+(x*(b*x+a)*exp(-(xi-b*x-a)^2/(2*sigma^2)))/(sqrt(2)*sqrt(pi)*sigma)))/(((b*x+a)*(1/2-erf((xi-b*x-a)/(sqrt(2)*sigma))/2)+(sigma*exp(-(xi-b*x-a)^2/(2*sigma^2)))/(sqrt(2)*sqrt(pi)))^2)
-((x*(xi-b*x-a)^3*exp(-(xi-b*x-a)^2/(2*sigma^2)))/(sqrt(2)*sqrt(pi)*sigma^4)+(x*(b*x+a)*(xi-b*x-a)^2*exp(-(xi-b*x-a)^2/(2*sigma^2)))/(sqrt(2)*sqrt(pi)*sigma^4)-(x*(b*x+a)*exp(-(xi-b*x-a)^2/(2*sigma^2)))/(sqrt(2)*sqrt(pi)*sigma^2))/((b*x+a)*(1/2-erf((xi-b*x-a)/(sqrt(2)*sigma))/2)+(sigma*exp(-(xi-b*x-a)^2/(2*sigma^2)))/(sqrt(2)*sqrt(pi)))
Hlr33<- (((xi-b*x-a)^2*exp(-(xi-b*x-a)^2/(2*sigma^2)))/(sqrt(2)*sqrt(pi)*sigma^2)+((b*x+a)*(xi-b*x-a)*exp(-(xi-b*x-a)^2/(2*sigma^2)))/(sqrt(2)*sqrt(pi)*sigma^2)+exp(-(xi-b*x-a)^2/(2*sigma^2))/(sqrt(2)*sqrt(pi)))^2/((b*x+a)*(1/2-erf((xi-b*x-a)/(sqrt(2)*sigma))/2)+(sigma*exp(-(xi-b*x-a)^2/(2*sigma^2)))/(sqrt(2)*sqrt(pi)))^2-(((xi-b*x-a)^4*exp(-(xi-b*x-a)^2/(2*sigma^2)))/(sqrt(2)*sqrt(pi)*sigma^5)+
((b*x+a)*(xi-b*x-a)^3*exp(-(xi-b*x-a)^2/(2*sigma^2)))/(sqrt(2)*sqrt(pi)*sigma^5)-(sqrt(2)*(xi-b*x-a)^2*exp(-(xi-b*x-a)^2/(2*sigma^2)))/(sqrt(pi)*sigma^3)+((xi-b*x-a)^2*exp(-(xi-b*x-a)^2/(2*sigma^2)))/(sqrt(2)*sqrt(pi)*sigma^3)-(sqrt(2)*(b*x+a)*(xi-b*x-a)*exp(-(xi-b*x-a)^2/(2*sigma^2)))/(sqrt(pi)*sigma^3))/
((b*x+a)*(1/2-erf((xi-b*x-a)/(sqrt(2)*sigma))/2)+(sigma*exp(-(xi-b*x-a)^2/(2*sigma^2)))/(sqrt(2)*sqrt(pi)))
      Hlr<-matrix(NA,3,3)
      Hlr[1,1]=sum(Hlr11);
      Hlr[1,2]=sum(Hlr12);
      Hlr[1,3]=sum(Hlr13);
      Hlr[2,2]=sum(Hlr22);
      Hlr[2,3]=sum(Hlr23);
      Hlr[3,3]=sum(Hlr33);
      Hlr[2,1]<-Hlr[1,2];Hlr[3,2]<-Hlr[2,3];Hlr[3,1]<-Hlr[1,3];
      return(Hlf+Hlr)},
    ll=function(listeqtes,xi,theta,n){
        x<-listeqtes$y[,1];
        y<-listeqtes$y[,2];
        a<-theta[1];b<-theta[2];sigma<-theta[3];
        return(sum(
          log(y)-(y-b*x-a)^2/(2*sigma^2)-log(sqrt(2*pi))-
            log((b*x+a)*(1/2-erf((xi-b*x-a)/(sqrt(2)*sigma))/2)+
            (sigma*exp(-(xi-b*x-a)^2/(2*sigma^2)))/
            (sqrt(2)*sqrt(pi)))))},
    xihat=function(y,z,s){return(min(y[s,2]))},
    thetaht=function(y,z,s){
        sprime<-unique(s);
        piks<-SystematicPPS(sampleparam)$Pik(z)[sprime];
        lmm<-lm(y[sprime,2]~cbind(y[sprime,1]),weights=1/piks);
        sigmahat<-sum(lmm$residuals^2/piks)/sum(1/piks)
        return(c(as.vector(lmm$coefficients),sigmahat))},
    thetaniais=function(y,z,s){
        sprime<-s#unique(s);
        lmm<-lm(y[sprime,2]~cbind(y[sprime,1]));
        return(c(as.vector(lmm$coefficients),summary(lmm)$sigma))},
    thetahat=function(y,z,s){},
    supportY=c(-.5,5),
    En=function(N){tau*N},
    tau=tau,
    I11=cave$I11,
    I12=cave$I12,
    V=cave$V,
    VHT=cave$VHT,
    Vniais=cave$Vniais,
    Sigma=cave$Sigma))}

