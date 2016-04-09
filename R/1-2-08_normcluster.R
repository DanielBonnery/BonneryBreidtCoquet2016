
model.norm.cluster<-function(sampleparam,xi,theta,param){
  proph=sampleparam$proph;probh=sampleparam$probh;tauh=sampleparam$tauh
  tau=sum(proph*probh*tauh)
  Ninf=100000;Nhinf=floor(Ninf*proph);
  yinf<-rnorm(Ninf)+2;
  sinf<-StratS(list(proph=proph*probh,tauh=tauh))$S(xi*yinf+rnorm(Ninf));
  dloilim<-densite(yinf[sinf])
  return(list(
  sampleparam=sampleparam,
  theta=theta,
  xi=xi,
  param=param,
  rloiy=function(N){rnorm(N,2)},
  ploi=function(y){pnorm(y,2)},
  rloiz=function(y){rnorm(y,mean=xi*y,sd=1)},
  dloi=function(y){dnorm(y,2)},
  #ploilim=FDR(yinf[sinf]),
  dloilim=dloilim,
  Scheme=ClusterS(proph,tauh,probh),
  rho=function(y){dloilim(y)/dnorm(y,2)},
  vinf=function(y){NULL},
  En=function(N){tau*N},
  tau=tau,
  supportY=c(-.5,4)))}

