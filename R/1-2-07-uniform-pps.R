

#2.2. Uniform pps

model.unif.pps<-function(sampleparam,theta,xi){
  paramtau<-param$tau
  return(
  list(rloiy=function(N){2*runif(N)},
    ploi=function(y){punif(y/2)},
    ploilim=function(y){y/2*(y>0)*(y<=2)},
    rloiz=function(y){y},
    dloi=function(y){dunif(y/2)/2},
    Scheme=SWRPPS(param),
    rho=function(y){y},
    rhothetaxi=function(y,theta,xi){y},
    vinf=function(y){paramtau*y},
    En=function(N){paramtau*N},
    tau=param$tau,
    supportY=c(-.1,2.1)))}
#"test :";param<-list(tau=c(.5));m<-model.unif.pps(param);m$ploi(2);m$rloiy(2);m$ploilim(2);m$dloi(2:3);m$Scheme;m$Scheme$S(5:15)
model.Pfeffermann.syst1<-function(sampleparam,theta,xi,param){
  g<-function(x,y,u,xi){exp(xi[1]*y+xi[2]*y^2+xi[3]*x^2+xi[4]*u)}
  return(list(rloiy=function(N){x<-rgamma(N,1,1);matrix(c(x,1+x+rnorm(N)),N,2)},
  ploi=function(x,y){
    if(is.vector(x,y)){p<-pnorm(y[3]-theta[1]-theta[2]*y[1],sd=xi[5])}
    if(is.matrix(x,y)){p<-pnorm(y[,2]-theta[1]-theta[,2]*y[1])}
    return(p)
    },
  ploilim=function(x,y){
    if(is.vector(x,y)){p<-pnorm(y[3]-theta[1]-theta[2]*y[1],sd=xi[5])}
    if(is.matrix(x,y)){p<-pnorm(y[,2]-theta[1]-theta[,2]*y[1])}
    return(p)},
  rloiz=function(x,y){g<-function(x,y,u,xi){exp(xi[1]*y+xi[2]*y^2+xi[3]*x^2+xi[4]*u)}
    if(is.vector(x,y)){r<-g(y[1],y[3],rnorm(1),xi)}
    if(is.matrix(x,y)){r<-g(y[,1],y[,2],rnorm(length(y[,1])),xi)}
    return(r)},
  dloi=function(y){
    if(is.vector(x,y)){d<-dnorm(y[3]-theta[1]-theta[2]*y[1],sd=xi[5])}
    if(is.matrix(x,y)){d<-dnorm(y[,2]-theta[1]-theta[,2]*y[1])}
    return(d)},
  Scheme=SystematicPPS(sampleparam),
  rho=function(y){y},
    rhothetaxi=function(y,xi,theta){y},
  vinf=function(y){tau*y},
  En=function(N){tau*N},
  tau=tau,
  supportY=c(-.1,2.1)))}

