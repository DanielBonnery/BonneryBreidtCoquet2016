
##Chapitre 4. Kernel density estimation  
##4.1. Definitions
# Kernels
kergaus<-list(K=dnorm,intK2=(1/(2*sqrt(pi))));
ker<-kergaus

# bandwidths
b<-function(N){1/sqrt(N)}
varp<-function(y0,b,ker=ker,m=m,N){(m$dloi(y0)/(N*b(N)))*(m$vinf(y0)/(m$tau^2)+((m$rho(y0))^2))*ker$intK2}
varpsr<-function(y0,b,ker=ker,m=m,N){varp(y0,b,ker=ker,m=m,N)/((m$rho(y0))^2)}

# Kernel density estimators definition of kde
p0  <-function(y0,Gg,b,ker=ker,m=m){sum(ker$K((Gg$Yg[Gg$Sg]-y0)/b(Gg$N)))/(b(Gg$N)*Gg$n)}
fHT0<-function(y0,Gg,b,ker=ker,m=m){sum(ker$K((Gg$Yg[Gg$Sg]-y0)/hpi(x=Gg$Yg[Gg$Sg])*2)/Gg$pik[Gg$Sg])/(b(Gg$N)*Gg$NHT)*2}
p   <-function(y0,Gg,b,ker=ker,m=m){sapply(y0,p0,Gg=Gg,b=b,ker=ker,m=m)}
p2  <-function(y0,Gg,b,ker=ker,m=m){as.vector(kde(Gg$Yg[Gg$Sg],hpi(x=Gg$Yg[Gg$Sg])/2,eval.points=y0)$estimate)}
psr <-function(y0,Gg,b,ker=ker,m=m){p(y0,Gg,b,ker=ker,m=m)/m$rho(y0)}
p2sr<-function(y0,Gg,b,ker=ker,m=m){p2(y0,Gg,b,ker=ker,m=m)/m$rho(y0)}
fHT <-function(y0,Gg,b,ker=ker,m=m){sapply(y0,fHT0,Gg=Gg,b=b,ker=ker,m=m)}

# Calculus of variance
moments<-function(y0,b,ker=ker,m=m,lafun=psr,N=N,nrep=1000){
  XX<-matrix(NA,nrep,length(y0))
  for(i in 1:nrep){
  Gg<-genere(m,N)
  XX[i,]<-lafun(y0,Gg,b,ker=ker,m=m)}
  return(list(E=apply(XX,2,mean),var=apply(XX,2,var)))}
##Simulations and verification of variance formula
Verif<-function(m,N,b,nrep=100,nbpts=30,fic,verifvp){
  y0<-seq(m$support[1],m$support[2],length.out=nbpts);
  mmts<-moments(y0,b,ker=ker,m=m,lafun=p,N=N,nrep=nrep);
  vpemp<-mmts$var;
  vp<-varp(y0,b=b,ker=ker,m=m,N=N);
  png(paste(fic,"_1.png"))
    plot(y0,vpemp/vp,type='l',col="blue");
    title("v empirique/v theorique")
  dev.off()
  png(paste(fic,"_2.png"))
    plot(y0,vp,type='l',col="blue");
    points(y0,vpemp,type='l',col="orange");
    points(y0,verifvp(y0),type='l',col="red");
    title("v empirique(orange) - v theorique (bleu) - v theorique verif (rouge)") 
  dev.off()
  png(paste(fic,"_3.png"))
    plot(y0,m$dloi(y0),type='l',col="black");
    plot(y0,m$rho(y0)*m$dloi(y0),type='l',col="blue");
    points(y0,mmts$E,type='l',col="orange");
    title("E[p] empirique(orange) -  f (bleu)- rho f (noir)") 
  dev.off()
  png(paste(fic,"_4.png"))
    Gg<-genere(m,N)
    plot(y0,m$rho(y0)*m$dloi(y0),type='l',col="black");
    points(y0,p(y0,Gg,b,ker=ker,m=m),type='l',col="orange");
    title("p orange -  rho f (bleu)- rho f (noir)") 
    dev.off()
return(list(vp=vp,vpemp=vpemp,y0=y0,mmts=mmts,m=m,verifvp=verifvp,N=N,nrep=nrep))
}
