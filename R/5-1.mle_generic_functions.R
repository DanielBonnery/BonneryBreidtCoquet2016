#####################################################################
##  5.1 Maximum Likelihood Estimation generic functions
#####################################################################
#    Entries:
#      - m : a model, i.e. a list that contains at least
#              - the rhothetaxi function, that is a function of a 
#                vecteur y, and parameters theta and xi
#              - a density function dloitheta
#      - y : a vecteur of obsercations
#      - theta
#      - xi
#      - thetaxi: concatenation of theta and xi
###################################################################
## 1. Computations related to loglikelihood

##1.1 Calculus of the mean sample log likelihood function
sample.likelihood<-function(theta,y,model,xi){return(sum(log(model$rhothetaxi(y,theta,xi))+log(model$dloitheta(y,theta))))}
##1.2. Calculus of the mean population log likelihood
pop.likelihood<-function(theta,y,model,xi){return(sum(log(model$dloitheta(y,theta))))}
##1.3. Calculus of the derivative of the loglikelihood for one observation
loglikethetaxi <- function(thetaxi,model,y){
  log(model$rhothetaxi(y,thetaxi[1:length(model$theta)],thetaxi[length(model$theta)+1:length(model$xi)])*model$dloitheta(y,thetaxi[1:length(model$theta)]))}
deriveloglikethetaxi  <- function(y,model,theta,xi){numDeriv::jacobian(c(theta,xi),func=loglikethetaxi,model=model,y=y)}
##1.4 Second order derivative for one observation
deriveloglikethetaxi2 <- function(y,model,theta,xi){numDeriv::hessian (c(theta,xi),func=loglikethetaxi,model=model,y=y)}
##1.5 Second order derivative for one observation          
rhoderiveloglikethetaxi2<-function(y,model,theta,xi){
  model$rhothetaxi(y,theta,xi)*deriveloglikethetaxi2(y,model,theta,xi)}
##1.6 Version of previous functions for many observations  
Deriveloglikethetaxi <- deriveloglikethetaxi
#function(y,model,theta,xi){
# if(is.matrix(y)){return(t(apply(y,MARGIN=1,deriveloglikethetaxi,model=model,theta=theta,xi=xi)))} 
# if(is.vector(y)){return(deriveloglikethetaxi(y,model,theta,xi))}}

Deriveloglikethetaxi2 <-function(y,model,theta,xi){
  if(is.vector(y)){return(deriveloglikethetaxi2(y,model,theta,xi))} 
  if(is.matrix(y)){
    return(array(unlist(lapply(lapply(seq_len(length(y[,1])),function(i){y [i,]}),
                               deriveloglikethetaxi2,model=model,theta=theta,xi=xi)),
                 dim=c(4,4,length(y[,1]))))}}

RhoDeriveloglikethetaxi2<-function(y,model,theta,xi){
  if(is.vector(y)){return(deriveloglikethetaxi2(y,model=model,theta,xi))} 
  if(is.matrix(y)){
    return(array(unlist(lapply(lapply(seq_len(length(y[,1])),function(i){y [i,]}),
                               rhoderiveloglikethetaxi2,model=model,theta=theta,xi=xi)),
                 dim=c(4,4,length(y[,1]))))}}   
## Computation of information matrices
Imatrix<-function(N,model,nbrepI=300){#3 minutes for n=30
  x <- rloix(N)
  return(apply(array(unlist(lapply(1:nbrepI,
                                   function(i){
                                     y<-cbind(x,rloiy.x(x,N))
                                     dd<-Deriveloglikethetaxi(y,model,m$theta,m$xi)
                                     rhorho=m$rhothetaxi(y,m$theta,m$xi)
                                     return(t(dd)%*%(dd*rhorho)/N)})),
                     dim=c(4,4,nbrepI)),
               c(1,2),
               mean))}


#compute the information matrix in different ways
Imatrix2<-function(N,model,nbrepI=300){#very long, 9 minutes for n=30
  x <- rloix(N)
  xrep<-rep(x,nbrepI);
  nrep=length(xrep)
  y<-cbind(xrep,rloiy.x(xrep,N))
  return( -apply(RhoDeriveloglikethetaxi2(y,model,theta,xi),c(1,2),mean))}

Imatrix3<-function(N,model,nbrepI=300){
  x <- rloix(N)
  xrep<-rep(x,nbrepI);
  nrep=length(xrep)
  y<-cbind(xrep,rloiy.x(xrep,N))
  dd<-Deriveloglikethetaxi(y,model,model$theta,model$xi)
  rhorho=model$rhothetaxi(y,model$theta,model$xi)
  return(t(dd)%*%(dd*rhorho)/nrep)}

Imatrix6<-function(N,model,nbrepI=300){
  x <- rloix(N)
  xrep<-rep(x,nbrepI);
  y<-cbind(xrep,rloiy.x(xrep,N))
  s<-Scheme$S(rloiz(y))
  nrep=length(s)
  dd<-Deriveloglikethetaxi(y[s,],model,theta,xi)
  return(t(dd)%*%(dd)/nrep)}


Imatrix9<-function(N,model,nbrepI=3000){
  x <- model$rloix(N)
  xrep<-rep(x,nbrepI);
  y<-model$rloiy.x(xrep,N)
  dd<-Deriveloglikethetaxi(y,model,model$theta,model$xi)
  rhorho <- model$rhothetaxi(y,model$theta,model$xi)*model$rhoxthetaxi(xrep,model$theta,model$xi)
  return(t(dd)%*%(dd*rhorho)/(N*nbrepI))}




Imatrix7<-function(N,model,nbrepI=300){
  x <- rloix(N)
  xrep<-rep(x,nbrepI);
  y<-cbind(xrep,rloiy.x(xrep,N))
  nrep=length(s)
  s<-Scheme$S(rloiz(y))
  dd<-Deriveloglikethetaxi(y[s,],model,theta,xi)
  return(t(dd)%*%(dd)/nrep)}

Imatrix4<-function(N,model,nbrepI=300){#very slow
  x <- rloix(N)
  xrep<-rep(x,nbrepI)
  y<-cbind(xrep,rloiy.x(xrep,N))
  s<-Scheme$S(rloiz(y)) #draw a sample
  return( -apply(Deriveloglikethetaxi2(y[s,],model,theta,xi),c(1,2),mean))}

Imatrix5<-function(N,model,nbrepI=300){#very quick, different
  x <- rloix(N)
  return(apply(array(
    unlist(
      lapply(seq_len(nbrepI),
             function(i){
               y<-cbind(x,rloiy.x(x,N))
               s<-Scheme$S(rloiz(y)) 
               return( -apply(Deriveloglikethetaxi2(y[s,],model,theta,xi),c(1,2),mean))})),
    dim=c(4,4,nbrepI)),
    c(1,2),mean))}

if(FALSE){
  N<-5000;nbrepI=300
  
  system.time(I1 <- Imatrix(N,model,nbrepI));#user 25 s
  system.time(I2 <- Imatrix2(N,model,nbrepI=300));#user :14060
  system.time(I3 <- Imatrix3(N,model,nbrepI));#user 18s
  system.time(I4 <- Imatrix4(N,model,nbrepI=300));#user 12950
  system.time(I5 <- Imatrix5(N,model,nbrepI))#user 750
  lapply(paste("I",1:5,sep=""),get);
}


calcule.Sigma<-function(model,N,nbrepSigma=1000){
  x <- model$rloix(N)
  dime<-c(length(model$theta),model$xihatfuncdim,length(model$theta)+model$xihatfuncdim,nbrepSigma)
  return(N *model$tau*
           var(t(apply(array(unlist(
             lapply(1:nbrepSigma,
                    function(qcq){
                      y=model$rloiy.x(x,N)#generates y conditionnally to x
                      z=model$rloiz(y)# generates z conditionnally to x and y
                      s <- model$Scheme$S(z);# draws the sample
                      pi <- model$Scheme$Pik(z);# compute the inclusion probabilities            
                      return(apply(cbind(Deriveloglikethetaxi(as.matrix(y)[s,],model,model$theta,model$xi),
                                         model$xihatfunc1(as.matrix(y)[s,],z[s],pi[s])),2,mean))}))
             ,dim=dime[c(3,4)]),
             2,function(u){c(u[1:dime[1]],model$xihatfunc2(u[dime[1]+1:dime[2]]))}))))}

calculeV<-function(Sigma,Im,dimtheta){  
  dimxi<-length(Sigma[1,])-dimtheta
  Sigma11<-Sigma[1:dimtheta,1:dimtheta];
  Sigma22<-Sigma[dimtheta+dimxi,dimtheta+dimxi];
  Sigma12<-Sigma[1:dimtheta,dimtheta+dimxi]
  I11<-Im[1:dimtheta,1:dimtheta];
  I12<- as.matrix(Im[1:dimtheta,dimtheta+dimxi])
  
  V <-   solve(I11)%*%(Sigma11+I12%*%Sigma22%*%t(I12)-I12%*%t(Sigma12)-Sigma12%*%t(I12))%*%solve(I11)
  V1 <-  solve(I11)%*%(Sigma11)%*%solve(I11)
  V2 <-  solve(I11)%*%(I12%*%Sigma22%*%t(I12))%*%solve(I11)
  V3 <-  solve(I11)%*%(-I12%*%t(Sigma12)-Sigma12%*%t(I12))%*%solve(I11)
  return(list(V=V,V1=V1,V2=V2,V3=V3))}


cav<-function(model,N,nbrepSigma=300,nbrepI=300){
  Sigma <- calcule.Sigma(model,N,nbrepSigma)
  Im <- Imatrix9(N,model)
  dimtheta<-length(model$theta);
  V123<-calculeV(Sigma,Im,dimtheta)
  V <- V123$V/(N*model$tau)
  VHT<-NA;#(sum(tauh*proph))*sum(tauh*proph*Vh.ypi_1)
  Vniais<-NA;#1/(sum(tauh*proph))*sum(tauh*proph*Vh.y)
  return(list(Sigma=Sigma,Im=Im,VHT=VHT,V=V,Vniais=Vniais,
              V1=V123$V1/(N*model$tau),V2=V123$V2/(N*model$tau),V3=V123$V3/(N*model$tau)))}


##5. Optimisation procedure : computation of the maximum likelihood estimator

optim<-function(y,z,s,model,prec,method){
  #y<-Yg;z<-Zg;s<-Sg
  if(is.vector(y)){N<-length(y)}
  if(is.matrix(y)){N<-length(y[,1])}
  if(method=="steepest"){
    thetahat<-model$theta;
    xihat<-model$xihat(y,z,s,model$Scheme$Pik(z));
    if(!is.matrix(y)){
      listeqtes<-model$calculsintermediairespourjac(y[s])}
    if(is.matrix(y)){
      listeqtes<-model$calculsintermediairespourjac(y[s,])}
    k<-1;n<-length(y[,1]);
    continue<-TRUE
    alpha<-1
    while(continue){
      Jac<-model$Jacobiane(listeqtes,xihat,thetahat,n)
      delta<-delta*min(abs(thetahat))/(2*max(abs(delta)))
      fff<-function(xx){
        model$ll(listeqtes,xihat,thetahat+xx*Jac,n)
      }
      xx<-seq(-alpha,alpha,length.out=50)  
      yy<-sapply(xx,fff);yy[is.na(yy)]<--Inf
      alpha<-alpha/2+abs(unique(xx[yy==max(yy)])/2)
      plot(xx,yy,type='l')
      abline(a=model$ll(listeqtes,xihat,thetahat,n),b=sum(Jac^2),col='red')
      thetahat0<-thetahat;
      thetahat<-thetahat+unique(xx[yy==max(yy)])*Jac;
      points(unique(xx[yy==max(yy)]),max(yy))
      thetahat0;thetahat;model$ll(listeqtes,xihat,thetahat,n);delta;alpha;
      sum(Jac^2)
      k<-k+1;
      continue<-(max(abs(delta))>prec)&(k<50)
      if(is.na(continue)){continue<-FALSE}}}
  if(method=="NewtonR"){
    thetahat<-model$theta;
    xihat<-model$xihat(y,z,s,model$Scheme$Pik(z));
    listeqtes<-model$calculsintermediairespourjac(y)
    continue<-TRUE
    k<-1;n<-length(y[,1]);
    continue<-TRUE
    k<-1
    while(continue){
      delta<-solve(model$hessiane(listeqtes,xihat,thetahat,n))%*%model$Jacobiane(listeqtes,xihat,thetahat,n)
      thetahat<-thetahat-delta;
      k<-k+1;
      continue<-(sqrt(sum(delta^2))>prec)&(k<30)
      if(is.na(continue)){continue<-FALSE}}}
  if(method=="grille"){
    if(is.vector(y)){ys<-y[s]}
    if(is.matrix(y)){ys<-y[s,]}                 #vector of sample responses
    xihat<-model$xihat(y,z,s,model$Scheme$Pik(z));
    thetahat=optimize(f=sample.likelihood,interval=c(0.2,5)*model$theta,y=ys,model=model,xi=xihat,maximum=TRUE)$maximum
    if (model$V!=0){thetahat=optimize(f=sample.likelihood,interval=(1+(c(-.1,.1)*sqrt(model$V/(model$tau*N))))*thetahat,tol=sqrt(model$V)/1e8,y=ys,model=model,xi=xihat,maximum=TRUE)$maximum}
  }
  if(method=="grilleIt"||method=="grilleItpop"){
    
    if(is.vector(y)){ys<-y[s]}
    if(is.matrix(y)){ys<-y[s,]}                 #vector of sample responses
    xihat<-model$xihat(y,z,s,model$Scheme$Pik(z));
    
    if(method=="grilleIt"){sl<-sample.likelihood}
    if(method=="grilleItpop"){sl<-pop.likelihood}
    
    if(FALSE){
      samplelike<-function(x){return(-sample.likelihood(x,ys,model,xihat))}
      gr<-function(x){return(as.vector(model$Jacobiane(model$calculsintermediairespourjac(ys),xihat,x,length(ys[,1]))))}
      hess<-function(x){return(as.vector(model$hessiane(model$calculsintermediairespourjac(ys),xihat,x,length(ys[,1]))))}
      optimx(par=model$theta,fn=samplelike,gr=gr,hess=hess)}
    
    
    
    if(TRUE){
      thetahat<-model$theta
      th<-vector()
      dif=1;k=1;dif2=1
      while(dif>10^{-7}&&k<10000){
        thetahat0<-thetahat
        for (i in 1:length(model$theta)){
          samplelike<-function(x){
            return(-sl(thetahat+(x-thetahat[i])*((1:length(model$theta))==i),ys,model,xihat))}
          thetahat[i]<-optimize(f=samplelike,interval=c(-1.5,1.5)*dif2+thetahat[i],tol=dif/1000)$minimum
        } 
        k<-k+1
        dif=max(abs(thetahat0-thetahat))
        dif2=max(abs(thetahat0-thetahat))+(max(abs(model$theta-thetahat))==0)*(.1^(k/20))
        th<-cbind(th,thetahat)
      }}}
  
  if(method=="expression"){
    thetahat<-model$thetahat(y,model,xi.hat)}
  return(thetahat)
}

#6. Simulation procedure
# Entry :

#  - m a population model, i.e. a list that contains at least:
#      - a function rloiy to generate the population study variable
#      - a function rloiz that generates the population design variable
#      - Scheme a list that contains
#        - a function Pik
#        - a Scheme function S that given a vector of design variables
#          returns a random sample
#  - N a population size
#  - nbreps : number of replicates
#  - method : name of the method for optimisation
#    ("grille", "Grille It", 

simule<-function(N,model,method,nbreps=300){
  #Set the precision (used in optimisation procedure)
  prec<-0.000001
  attach(model)
  attach(Scheme)
  #initialization : those vectors will contain the values of the 
  Xg<-rloix(N);
  Estim <- plyr::rlply(nbreps,
    (function(){
      #Population generation and sample selection
      Yg<-cbind(Xg,rloiy.x(X,N));  #Y generation
      Zg<-rloiz(Yg); #Z generation
      Sg<-S(Zg);     #sample selection
      Pikg<-Pik(Zg)
      return(list(xi.hat   =xihat     (Yg,Zg,Sg,Pikg),
                  theta.ht =thetaht   (Yg,Zg,Sg,Pikg),
                  theta.bar=thetaniais(Yg,Zg,Sg),
                  theta.hat=optim       (Yg,Zg,Sg,model,prec,method)))})())
  noms<-names(Estim[[1]])
  Estim<-lapply(as.list(noms),function(nom){plyr::laply(Estim,function(ll){ll[[nom]]})})
  names(Estim)<-noms
  attach(Estim)
  Var<-lapply(Estim,var)
  M=lapply(Estim,function(est){apply(as.matrix(est),2,mean)})
  E=list(model$xi,model$theta,model$theta,model$theta)
  names(E)<-names(M)
  Bias=lapply(as.list(noms),function(x){M[[x]]-E[[x]]})
  names(Bias)<-names(M)
  MSE=lapply(as.list(noms),function(x){Var[[x]]+Bias[[x]]%*%t(Bias[[x]])})
  names(MSE)<-names(M)
  return(list(
    model=model,
    Estim=Estim,
    Var=Var,
    M=M,
    Bias=Bias,
    MSE=MSE))}

##2. Function that displays a number 
#    for display of nice numbes in output tex tables)
affiche<-function(X){
  textee<-sapply(X,function(x){
    af<-"";
    if (is.na(x)){af<-" "}
    if(!is.na(x)){
      if (length(x)>0){
        puis2<-floor(log(abs(x))/log(10));
        puis<-floor(log(abs(x))/log(10^3));if (puis2>-3 &&puis2<3){puis=0};
        af<-paste("",signif(10^(-3*puis)*x,3),"\\ 10^{",3*puis,"}");
        if(puis>=0&puis<3){af<-paste("",signif(x,3),sep='')}
        if(x==0){af="0"}}}
    return(af)})
  aff<-textee
  if(is.matrix(X)&&length(X[,1])+length(X[1,])>2){aff<-affmatrix(matrix(textee,length(X[1,]),length(X[,1])),X)}
  if(is.vector(X)&&length(X)>1){aff<-affvector(textee)}
  return(aff)}

##3. Function to display a matrix
affmatrix<-function(textee,X){
  aff<-textee[,1]
  if(length(X[1,])>1){for(j in 2:length(X[1,])){aff<-paste(aff,paste(textee[,j]),sep="&")}}
  aff<-paste(aff,"\\\\",sep="")
  aff<-paste("\\begin{bmatrix}",paste(aff,collapse=""),"\\end{bmatrix}",sep="")
  return(aff)}

##4. function to display a vector
affvector<-function(textee){
  aff<-paste("\\begin{bmatrix}",paste(textee,collapse="\\\\"),"\\end{bmatrix}",sep="")
  return(aff)}



#7. Simulations and output
##    procedure that launches simulations and produces an output:
##    a tex code for a table containing the results of simulation

Simulation_data<-function(nbreps,popmodelfunction,sampleparam,N,theta,xi,param,method){
  model<-popmodelfunction(sampleparam,theta,xi,param)
  cave <- cav(model,N,nbrepSigma=1000,nbrepI=3000)
  sim<-simule(N=N,model,method,nbreps=3000)
  return(list(theta=theta,param=param,xi=xi,method=method,sim=sim,cave=cave))}

generetableau<-function(simulation_dataL,nomparam,fic=NULL,directory="."){
  if(is.null(fic)){filee<-tempfile()}else{filee<-file.path(directory,fic)}
  struct<-"c|c|rrrr|r|r";sepparam<-"&";slashparam<-"\\"
  if (nomparam==""){struct<-"c|rrrr|r|r";sepparam<-"";slashparam<-""}
  write(paste("\\batchmode\\documentclass[10pt]{report}
              \\usepackage[landscape]{geometry}
              \\usepackage[utf8]{inputenc}
              \\usepackage{amsmath}
              \\newcommand{\\E}[1]{\\mathrm{E}_{#1}}
                \\begin{document}
              \\begin{tabular}{",struct,"}",sep=''),file=filee,append=F)
  write("\\hline",file=filee,append=T)
  write(paste(nomparam,sepparam,
              "$\\","theta=",affiche(simulation_data[[1]]$theta),"$
      &Mean[.]
      &Biais[.]
      &Empirical variance[.]
      &M.S.E[.] 
      &$\\sqrt{\\frac{\\rm{MSE}}{\\rm{MSE}(\\hat{\\theta})}}$
      &$\\frac{\\lim\\limits_{\\gamma\\to\\infty}\\V{\\sqrt{n_{\\gamma}}\\times.}}{\\E{n_{\\gamma}}}$\\\\\\hline",sep=''),file=filee,append=T)
  for(k in 1:length(simulation_data)){
    attach(simulation_data[[k]])
    attach(sim)
    write(paste("$\ ",affiche(param),"$", sepparam,
                "$\\hat{\\theta}$
        &$",affiche(M$theta.hat),"$ 
        &$",affiche(Bias$theta.hat),"$ 
        &$",affiche(diag(as.matrix(Var$theta.hat))),"$
        &$",affiche(diag(as.matrix(MSE$theta.hat))),"$&$1$
        &$",affiche(diag(as.matrix(cave$V))),"$
        \\\\"),file=filee,append=T)
    write(paste(sepparam,
                "$\\tilde{\\theta}$
        &$",affiche(M$theta.ht),"$ 
        &$",affiche(Bias$theta.ht),"$ 
        &$",affiche(diag(as.matrix(Var$theta.ht))),"$
        &$",affiche(diag(as.matrix(MSE$theta.ht))),"$
        &$",affiche(diag((as.matrix(sqrt(MSE$theta.ht/MSE$theta.hat))))),"$
        &$",affiche(diag(as.matrix(cave$VHT))),"$ \\\\"),file=filee,append=T)
    write(paste(sepparam,
                "$\\bar{\\theta}$
        &$",affiche(M$theta.bar),"$ 
        &$",affiche(Bias$theta.bar),"$ 
        &$",affiche(diag(as.matrix(Var$theta.bar))),"$
        &$",affiche(diag(as.matrix(MSE$theta.bar))),"$
        &$",affiche(diag(as.matrix(sqrt(MSE$theta.bar/MSE$theta.hat)))),"$
        &$",affiche(diag(as.matrix(cave$Vniais))),"$ \\\\\\hline"),file=filee,append=T)}
  write("\\end{tabular}\\end{document}",file=filee,append=T)
  try(system(paste0("cd ",dirname(filee),"&& pdflatex ",basename(filee))))
  try(system(paste0("cd ",dirname(filee),"&& evince ",basename(filee),".pdf")))
  cat(readLines(filee))
  return(file.path(dirname(filee),paste0(basename(filee),".pdf")))}

Verifvar<-function(model,N,method){
  list(cav(model,N)$V,simule(N,model,method)$var.hat)
}
