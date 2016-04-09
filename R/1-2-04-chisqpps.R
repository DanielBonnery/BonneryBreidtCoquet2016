#Chi2 pps
model.chisq2.pps<-function(tau){
  list(rloiy=function(N){rchisq(N,1)},
  ploi=function(y){pchisq(y,1)},
  ploilim=function(y){(pgamma(y,3/2,2)+pgamma(y,5/2,2))/2},
  rloiz=function(y){y+rchisq(N,1)},
  dloi=function(y){dchisq(y,1)},
  Scheme=SWRPPS(tau),
  rho=function(y){(y+1)/2},
  vinf=function(y){tau*y},
  En=function(N){tau*N},
  tau=tau,
  supportY=c(-.1,2.1))}
