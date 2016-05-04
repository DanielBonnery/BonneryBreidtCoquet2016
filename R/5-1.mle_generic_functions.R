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
##1.2 Calculus of the mean sample log likelihood function
full.likelihood<-function(thetaxi,y,model){if(!is.null(model$fulllikelihood)){log(model$fulllikelihood(y,thetaxi))}else{}}
##1.3. Calculus of the mean population log likelihood
pop.likelihood<-function(theta,y,model,xi){return(sum(log(model$dloitheta(y,theta))))}
##1.4. Calculus of the derivative of the loglikelihood for one observation
loglikethetaxi <- function(thetaxi,model,y){
  log(model$rhothetaxi(y,thetaxi[1:length(model$theta)],thetaxi[length(model$theta)+1:length(model$xi)])*model$dloitheta(y,thetaxi[1:length(model$theta)]))}
deriveloglikethetaxi  <- function(y,model,theta,xi){numDeriv::jacobian(c(theta,xi),func=loglikethetaxi,model=model,y=y)}
##1.5 Second order derivative for one observation
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
    return(plyr::aaply(y,1,rhoderiveloglikethetaxi2,model=model,theta=theta,xi=xi))}}   
## Computation of information matrices
Imatrix1<-function(N,model,nbrepI=300,x=NULL){#3 minutes for n=30
  x <- model$rloix(N)
  xx<-plyr::raply(nbrepI,
                  function(i){
                    y<-model$rloiy.x(xrep,N)
                    dd<-Deriveloglikethetaxi(y,model,model$theta,model$xi)
                    rhorho=model$rhothetaxi(y,model$theta,model$xi)
                    return(t(dd)%*%(dd*rhorho)/N)})
  return(apply(xx,2:length(dim(xx)),mean))}
#compute the information matrix in different ways
Imatrix2<-function(N,model,nbrepI=300,x=NULL){
  if(is.null(x)){x<-model$rloix(N)}
  xrep<-rep(x,nbrepI);
  nrep=length(xrep)
  y<-cbind(xrep,model$rloiy.x(xrep,N))
  xxx=RhoDeriveloglikethetaxi2(y,model,theta,xi)
  return( -apply(xxx,2:length(dim(xxx)),mean))}

Imatrix3<-function(N,model,nbrepI=300,x=NULL){
  if(is.null(x)){x<-model$rloix(N)}
  xrep<-rep(x,nbrepI);
  nrep=length(xrep)
  y<-cbind(xrep,model$rloiy.x(xrep,N))
  dd<-Deriveloglikethetaxi(y,model,model$theta,model$xi)
  rhorho=model$rhothetaxi(y,model$theta,model$xi)
  return(t(dd)%*%(dd*rhorho)/nrep)}

Imatrix4<-function(N,model,nbrepI=300,x=NULL){
  if(is.null(x)){x<-model$rloix(N)}
  xrep<-rep(x,nbrepI)
  y<-cbind(xrep,model$rloiy.x(xrep,N))
  s<-model$Scheme$S(model$rloiz(y)) #draw a sample
  return( -apply(Deriveloglikethetaxi2(y[s,],model,theta,xi),c(1,2),mean))}

Imatrix5<-function(N,model,nbrepI=300,x=NULL){
  if(is.null(x)){x<-model$rloix(N)}
  return(apply(array(
    unlist(
      lapply(seq_len(nbrepI),
             function(i){
               y<-cbind(x,model$rloiy.x(x,N))
               s<-model$Scheme$S(model$rloiz(y)) 
               return( -apply(Deriveloglikethetaxi2(y[s,],model,theta,xi),c(1,2),mean))})),
    dim=c(4,4,nbrepI)),
    c(1,2),mean))}

Imatrix6<-function(N,model,nbrepI=300,x=NULL){
  if(is.null(x)){x<-model$rloix(N)}
  xrep<-rep(x,nbrepI);
  y<-cbind(xrep,model$rloiy.x(xrep,N))
  s<-model$Scheme$S(model$rloiz(y))
  nrep=length(s)
  dd<-Deriveloglikethetaxi(y[s,],model,theta,xi)
  return(t(dd)%*%(dd)/nrep)}

Imatrix7<-function(N,model,nbrepI=300,x=NULL){
  if(is.null(x)){x<-model$rloix(N)}
  xrep<-rep(x,nbrepI);
  y<-cbind(xrep,model$rloiy.x(xrep,N))
  s<-model$Scheme$S(model$rloiz(y))
  dd<-Deriveloglikethetaxi(y[s,],model,model$theta,model$xi)
  return(t(dd)%*%(dd)/nrow(dd))}


Imatrix9<-function(N,model,nbrepI=3000,x=NULL){
  if(is.null(x)){x<-model$rloix(N)}
  xrep<-rep(x,nbrepI);
  y<-cbind(xrep,model$rloiy.x(xrep,N))
  dd<-Deriveloglikethetaxi(y,model,model$theta,model$xi)
  rhorho <- model$rhothetaxi(y,model$theta,model$xi)*model$rhoxthetaxi(xrep,model$theta,model$xi)
  return(t(dd)%*%(dd*rhorho)/(N*nbrepI))}




if(FALSE){
  N<-5000;nbrepI=300
  x <- model$rloix(N)
  compare<-sapply(sapply(paste("Imatrix",c(1:7,9),sep=""),get),function(x){
    try(timex<-system.time(I<- Imatrix1(N,model,nbrepI,x=x)))
    list(time=timex,I=I)    
  })
  system.time(I1 <- Imatrix1(N,model,nbrepI,x=x));#user 10.47 s
  system.time(I2 <- Imatrix2(N,model,nbrepI=300,x=x));#user :3.804
  system.time(I3 <- Imatrix3(N,model,nbrepI,x=x));#user 
  system.time(I4 <- Imatrix4(N,model,nbrepI=300,x=x));#user
  system.time(I5 <- Imatrix5(N,model,nbrepI,x=x))#user 
  system.time(I6 <- Imatrix6(N,model,nbrepI,x=x));#user 0.012
  system.time(I7 <- Imatrix7(N,model,nbrepI=300,x=x));#user 0.012
  system.time(I9 <- Imatrix9(N,model,nbrepI=300,x=x));#user 0.052
  lapply(paste("I",c(1:3,6,7,9),sep=""),get);
}


calcule.Sigma<-function(model,N,nbrepSigma=1000,x=NULL){
  if(is.null(x)){x <- model$rloix(N)}
  return(N *model$tau*
           var(plyr::aaply(
             plyr::raply(nbrepSigma,
                         function(){
                           y=model$rloiy.x(x,N)   #generates y conditionnally to x
                           z=model$rloiz(y)       #generates z conditionnally to x and y
                           s <- model$Scheme$S(z);#draws the sample
                           pi <- model$Scheme$Pik(z);# compute the inclusion probabilities            
                           return(apply(cbind(Deriveloglikethetaxi(as.matrix(y)[s,],model,model$theta,model$xi),
                                              model$xihatfunc1(as.matrix(y)[s,],z[s],pi[s])),2,mean))}),
             1,function(u){c(u[1:length(model$theta)],
                             model$xihatfunc2(u[length(model$theta)+length(model$xi)+(1:model$xihatfuncdim)]))})))}

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


cav<-function(model,N,nbrepSigma=300,nbrepI=300,x=NULL){
  if(is.null(x)){x <- model$rloix(N)}
  Sigma <- calcule.Sigma(model,N,nbrepSigma,x=x)
  Im <- Imatrix7(N,model,x=x)
  dimtheta<-length(model$theta);
  V123<-calculeV(Sigma,Im,dimtheta)
  V<-list(Sample=V123$V/(N*model$tau),Pseudo=NA,Naive=NA,Full=NA)
  return(list(Sigma=Sigma,Im=Im,V=V,
              V1=V123$V1/(N*model$tau),V2=V123$V2/(N*model$tau),V3=V123$V3/(N*model$tau)))}


##5. Optimisation procedure : computation of the maximum likelihood estimator
fullMLE<-function(y,z,s,model,method="nlm"){
  if(is.vector(y)){ys<-y[s]}
  if(is.matrix(y)){ys<-y[s,]}
  if(method=="formula"){model$fullMLE(y,z,s)}else{
    if(!is.null(model$fulllikelihood)){optimx::optimx(c(model$theta,model$xi),
                                                      fn =full.likelihood,control=list(maximize=TRUE,method="nlm"),model=model,y=ys,z=z)}else{NA}}}
sampleMLE<-function(y,z,s,model,method="nlm"){
  if(method=="formula"){model$sampleMLE(y,z,s)}else{
    if(is.vector(y)){ys<-y[s]}
    if(is.matrix(y)){ys<-y[s,]}
    xihat<-model$xihat(y,z,s,model$Scheme$Pik(z));
    unlist(optimx::optimx(model$theta,
                          fn=sample.likelihood,method=method,
                          control=list(maximize=TRUE),
                          y=ys,model=model,xi=xihat))[1:length(model$theta)]}}
pseudoMLE<-function(y,z,s,pi,model,method="nlm"){
  if(method=="formula"){model$sampleMLE(y,z,s)}else{
    if(is.vector(y)){ys<-y[s]}
    if(is.matrix(y)){ys<-y[s,]}
    xihat<-model$xihat(y,z,s,model$Scheme$Pik(z));
    unlist(optimx::optimx(model$theta,
                          fn=sample.likelihood,method=method,
                          control=list(maximize=TRUE),
                          y=ys,model=model,xi=xihat))[1:length(model$theta)]}}

#6. Simulation procedure
# Entry :

#  - model: a population model, i.e. a list that contains at least:
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

simule<-function(N,model,nbreps=300,method=list(Sample="nlm",Pseudo="nlm",Naive="nlm",Full="nlm"),x=NULL){
  #Set the precision (used in optimisation procedure)
  attach(model)
  attach(Scheme)
  #initialization : those vectors will contain the values of the 
  Xg<-if(is.null(x)){rloix(N)}else{x}
  Estim <- plyr::rlply(nbreps,
                       (function(){
                         #Population generation and sample selection
                         Yg<-cbind(Xg,rloiy.x(X,N));  #Y generation
                         Zg<-rloiz(Yg); #Z generation
                         Sg<-S(Zg);     #sample selection
                         Pikg<-Pik(Zg)
                         thetaxi.full=fullMLE(Yg,Zg,Sg,model,method)
                         return(list(xi.hat   =xihat     (Yg,Zg,Sg,Pikg),
                                     Pseudo =thetaht   (Yg,Zg,Sg,Pikg),
                                     Naive=thetaniais(Yg,Zg,Sg),
                                     Sample=sampleMLE(Yg,Zg,Sg,model,method),
                                     xi.full=thetaxi.full[length(model$theta)+(1:length(model$xi))],
                                     Full=thetaxi.full[1:length(model$theta)]))})())
  noms<-names(Estim[[1]])
  Estim<-lapply(as.list(noms),function(nom){plyr::laply(Estim,function(ll){ll[[nom]]})})
  names(Estim)<-noms
  attach(Estim)
  Var<-lapply(Estim,var)
  M=lapply(Estim,function(est){apply(as.matrix(est),2,mean)})
  E=list(model$xi,model$theta,model$theta,model$theta,model$xi,model$theta)
  names(E)<-names(M)
  Bias=lapply(as.list(noms),function(x){M[[x]]-E[[x]]})
  names(Bias)<-names(M)
  MSE=lapply(as.list(noms),function(x){Var[[x]]+Bias[[x]]%*%t(Bias[[x]])})
  names(MSE)<-names(M)
  return(list(
    model=model,
    Estim=Estim,
    Variance=Var,
    Mean=M,
    Bias=Bias,
    "M.S.E."=MSE))}

#7. Simulations and output
Simulation_data<-function(popmodelfunction,sampleparam,N,theta,xi,param,method="nlm",nbreps=3000,nbrepI=3000,nbrepSigma=1000){
  model<-popmodelfunction(sampleparam,theta,xi,param)
  x<-model$rloix(N);
  cave <- cav(model,N,nbrepSigma=nbrepSigma,nbrepI=nbrepI,x=x)
  sim<-simule(N=N,model,nbreps=nbreps,method=method,x=x)
  return(list(theta=theta,param=param,xi=xi,method=method,sim=sim,cave=cave))}

simulation.summary<-function(table_data){
  lapply(table_data,function(l){
    ll<-c(l$sim[c("Mean","Bias","Variance","M.S.E.")],l$cave["V"])
    X<-do.call(rbind,
            lapply(c("Naive","Pseudo","Sample","Full"),function(est){
              do.call(data.frame,c(list(Estimator=est),
                                   list("Mean"=as.array(list(signif(ll$Mean[est][[1]],3)))),
                                   list("% Relative Bias"=as.array(list(signif(100*ll$Bias[est][[1]]/ll$Mean[est][[1]],3)))),
                                   list("RMSE Ratio"=as.array(list(signif(diag(as.matrix(ll$"M.S.E."[est][[1]]))/diag(as.matrix(ll$"M.S.E"["Sample"][[1]],3)))))),
                                   list("Empirical Variance"=as.array(list(signif(diag(as.matrix(ll$Variance[est][[1]],3)))))),
                                   list("Asymptotic Variance"=as.array(list(signif(diag(as.matrix(ll$V[est][[1]],3))))))))}))
    names(X)<-c("Estimator","Mean","% Relative Bias","RMSE Ratio","Empirical Variance","Asymptotic Variance")
    X})}    




