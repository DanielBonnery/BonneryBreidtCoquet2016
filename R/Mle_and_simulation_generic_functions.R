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
##generate observations
generate.observations<-function(model){
  Y<-model$rloiy();  #Y generation
  Z<-rloiz(Y); #Z generation
  S<-model$Scheme$S(Z);     #sample selection
  Pik<-model$Scheme$Pik(Z)
  list(y=Y[S],z=Z[S],pik=Pik[S])}
## 1. Computations related to loglikelihood
##1.1 Calculus of the plugin sample log likelihood function
sample.loglikelihood.plugin<-function(theta,y,model,xi){sum(log(model$rhothetaxi(y,theta,xi))+log(model$dloitheta(y,theta)))}
##1.2 Calculus of the  sample log likelihood function
sample.loglikelihood<-function(thetaxi,obs,model){model$sampleloglikelihood(Obs,theta=thetaxi[1:length(model$theta)],xi=thetaxi[length(model$theta)+(1:length(model$xi))])}
full.loglikelihood<-function(thetaxi,obs,model){model$fullloglikelihood(Obs,theta=thetaxi[1:length(model$theta)],xi=thetaxi[length(model$theta)+(1:length(model$xi))])}
##1.3. Calculus of the mean population log likelihood
pop.loglikelihood<-function(theta,y,model,xi){return(sum(log(model$dloitheta(y,theta))))}
##1.4. Calculus of the derivative of the loglikelihood for one observation
loglikethetaxi <- function(thetaxi,model,y){
  log(model$rhothetaxi(y,thetaxi[1:length(model$theta)],thetaxi[length(model$theta)+1:length(model$xi)])*model$dloitheta(y,thetaxi[1:length(model$theta)]))}
deriveloglikethetaxi  <- function(y,model,theta,xi){
  numDeriv::jacobian(c(theta,xi),func=loglikethetaxi,model=model,y=y)}
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
  if(is.matrix(y)){return(plyr::aaply(y,1,deriveloglikethetaxi2,model=model,theta=theta,xi=xi))}}

RhoDeriveloglikethetaxi2<-function(y,model,theta,xi){
  if(is.vector(y)){return(deriveloglikethetaxi2(y,model=model,theta,xi))} 
  if(is.matrix(y)){return(plyr::aaply(y,1,rhoderiveloglikethetaxi2,model=model,theta=theta,xi=xi))}}   
## Computation of information matrices

Imatrixf<-function(model,nbrepI=300,method=NULL){
  if(is.null(method)){method<-"FirstDeriv"}
  if(method=="FirstDeriv"){Imatrix7(model,nbrepI)}else{
    if(method=="formula"&!is.null(model$cave$Imatrix)){model$cave$Imatrix}}}

Imatrix1<-function(model,nbrepI=300){#3 minutes for n=30
  xx<-plyr::raply(nbrepI,
                  (function(){
                    y<-model$rloiy()
                    dd<-Deriveloglikethetaxi(y,model,model$theta,model$xi)
                    rhorho=model$rhothetaxi(y,model$theta,model$xi)
                    return(t(dd)%*%(dd*rhorho)/model$conditionalto$N)})())
  return(apply(xx,2:length(dim(xx)),mean))}
#compute the information matrix in different ways
Imatrix2<-function(model,nbrepI=300){
  y<-do.call(rbind,plyr::rlply(nbrepI,as.matrix(model$rloiy())))
  xxx=RhoDeriveloglikethetaxi2(y,model,theta,xi)
  return( -apply(xxx,2:length(dim(xxx)),mean))}

Imatrix3<-function(model,nbrepI=300){
  y<-do.call(rbind,plyr::rlply(nbrepI,as.matrix(model$rloiy())))
  dd<-Deriveloglikethetaxi(y,model,model$theta,model$xi)
  rhorho=model$rhothetaxi(y,model$theta,model$xi)
  return(t(dd)%*%(dd*rhorho)/(nbrepI*N))}

Imatrix4<-function(model,nbrepI=300){
  y<-do.call(rbind,plyr::rlply(nbrepI,as.matrix(model$rloiy())))
  s<-model$Scheme$S(model$rloiz(y)) #draw a sample
  return( -apply(Deriveloglikethetaxi2(as.matrix(y)[s,],model,theta,xi),c(1,2),mean))}

Imatrix6<-function(model,nbrepI=300){
  y<-do.call(rbind,plyr::rlply(nbrepI,as.matrix(model$rloiy())))
  s<-model$Scheme$S(model$rloiz(y))
  nrep=length(s)
  dd<-Deriveloglikethetaxi(y[s,],model,theta,xi)
  return(t(dd)%*%(dd)/nrep)}

Imatrix7<-function(model,nbrepI=300){
  y<-do.call(rbind,plyr::rlply(nbrepI,as.matrix(model$rloiy())))
  s<-model$Scheme$S(model$rloiz(y))
  dd<-Deriveloglikethetaxi(as.matrix(y)[s,],model,model$theta,model$xi)
  return(t(dd)%*%(dd)/nrow(dd))}


Imatrix9<-function(model,nbrepI=300){
  y<-do.call(rbind,plyr::rlply(nbrepI,model$rloiy()))
  dd<-Deriveloglikethetaxi(y,model,model$theta,model$xi)
  rhorho <- model$rhothetaxi(y,model$theta,model$xi)*model$rhoxthetaxi(y,model$theta,model$xi)
  return(t(dd)%*%(dd*rhorho)/(model$conditionalto$N*nbrepI))}




if(FALSE){
  N<-5000;nbrepI=300
  compare<-sapply(sapply(paste("Imatrix",c(1:7,9),sep=""),get),function(x){
    try(timex<-system.time(I<- x(N,model,nbrepI,x=x)))
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


calcule.Sigma<-function(model,nbrepSigma=1000,method=list(I="MC",Sigma="MC")){
  attach(model$conditionalto)
  return(N *model$tau*
           var(plyr::aaply(
             plyr::raply(nbrepSigma,
                         function(){
                           y=model$rloiy()   #generates y conditionnally to x
                           z=model$rloiz(y)       #generates z conditionnally to x and y
                           s <- model$Scheme$S(z);#draws the sample
                           pi <- model$Scheme$Pik(z);# compute the inclusion probabilities            
                           return(apply(cbind(Deriveloglikethetaxi(as.matrix(y)[s,],model,model$theta,model$xi),
                                              model$xihatfunc1(as.matrix(y)[s,],as.matrix(z)[s,],pi[s])),2,mean))}),
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


cav<-function(model,nbrepSigma=300,nbrepI=300,method=list(Sigma="MC")){
  method<-list(Sigma=if(is.null(method$Sigma)){if(!is.null(model$Sigma)){"formula"}else{"MC"}}else{method$Sigma},
               I=if(is.null(method$Sigma)){if(!is.null(model$I)){"formula"}else{"FirstDeriv"}}else{method$I})
  attach(model$conditionalto)
  Sigma <- calcule.Sigma(model,nbrepSigma,method$Sigma)
  Imatrix <- Imatrixf(model,method=method$I)
  V123<-calculeV(Sigma,Imatrix,length(model$theta))
  V<-list(Sample=V123$V/(N*model$tau),Pseudo=NA,Naive=NA,Full=NA)
  return(list(Sigma=Sigma,Im=Imatrix,V=V,
              V1=V123$V1/(N*model$tau),V2=V123$V2/(N*model$tau),V3=V123$V3/(N*model$tau)))}


##5. Optimisation procedure : computation of the maximum likelihood estimator
fullMLE<-function(Obs,model,method=NULL){
  if(is.null(method)){method="nlm"}
  ys<-as.matrix(Obs$y)
  if(method=="formula"){model$fullMLE(Obs)}else{
    if(!is.null(model$fulllikelihood)){optimx::optimx(c(model$theta,model$xi),
                                                      fn =full.loglikelihood,control=list(maximize=TRUE,method=method),model=model,Obs=Obs)}else{NA}}}
sampleMLE<-function(Obs,model,method=NULL,xi.hat=NULL){
  if(is.null(method)){method="nlm"}
  if(method=="formula"){model$sampleMLE(Obs)}else{
    if(is.null(xi.hat)){xi.hat<-model$xihat(Obs)}
    unlist(optimx::optimx(model$theta,
                          fn=sample.loglikelihood.plugin,method=method,
                          control=list(maximize=TRUE),
                          y=Obs$y,model=model,xi=xi.hat))[1:length(model$theta)]}}
pseudoMLE<-function(Obs,model,method=NULL){
  if(is.null(method)){method="nlm"}
  if(method=="formula"){model$pseudoMLE(Obs)}else{
    unlist(optimx::optimx(model$theta,
                          fn=pseudo.loglikelihood,method=method,
                          control=list(maximize=TRUE),
                          y=Obs$y,model=model))}}

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

simule<-function(model,
                 nbreps=300,
                 method=NULL){
  method=list(Sample=if(is.null(method$Sample)){"nlm"}else{method$Sample},
              Pseudo=if(is.null(method$Pseudo)){if(is.null(model$thetaht))   {"nlm"}else{"formula"}}else{method$Pseudo},
              Naive =if(is.null(method$Naive )){if(is.null(model$thetaniais)){"nlm"}else{"formula"}}else{method$Naive },
              Full  =if(is.null(method$Full  )){if(is.null(model$thetafull)) {"nlm"}else{"formula"}}else{method$Full  })
  #Set the precision (used in optimisation procedure)
  attach(model)
  attach(Scheme)
  #initialization : those vectors will contain the values of the 
  Estim <- plyr::rlply(nbreps,
                       (function(){
                         #Population generation and sample selection
                         Obs<-generate.observations(model)
                         thetaxi.full=fullMLE(Obs,model,method$Full)
                         xi.hat   =xihat     (Obs)
                         return(list(xi.hat   =xi.hat,
                                     Pseudo =thetaht(Obs),
                                     Naive=thetaniais(Obs),
                                     Sample=try(sampleMLE(Obs,model,method$Sample,xi.hat=xi.hat)),
                                     xi.full=thetaxi.full[length(model$theta)+(1:length(model$xi))],
                                     Full=thetaxi.full[1:length(model$theta)]))})())
  noms<-names(Estim[[1]])
  Estim<-Estim[sapply(Estim,function(l){!is.character(l$Sample)})]
  Estim<-lapply(as.list(noms),function(nom){plyr::laply(Estim,function(ll){ll[[nom]]})})
  names(Estim)<-noms
  attach(Estim)
  Var<-lapply(Estim,var,na.rm=TRUE)
  M=lapply(Estim,function(est){apply(as.matrix(est),2,mean,na.rm=TRUE)})
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
    E=E,
    "M.S.E."=MSE))}

#7. Simulations and output
Simulation_data<-function(popmodelfunction,sampleparam,theta,xi,conditionalto,
                          method=NULL,nbreps=3000,nbrepI=3000,nbrepSigma=1000){
  model<-popmodelfunction(theta,xi,conditionalto)
  cave <- cav(model,nbrepSigma=nbrepSigma,nbrepI=nbrepI,method)
  sim<-simule(model,nbreps=nbreps,method)
  return(list(model=model,xi=xi,sim=sim,cave=cave))}

simulation.summary<-function(table_data){
  lapply(table_data,function(l){
    ll<-c(l$sim[c("Mean","Bias","Variance","M.S.E.","E")],l$cave["V"])
    X<-do.call(rbind,
               lapply(c("Naive","Pseudo","Sample","Full"),function(est){
                 do.call(data.frame,c(list(Estimator=est),
                                      list("theta"=as.array(list(l$model$theta))),
                                      list("xi"=as.array(list(l$model$xi))),
                                      list("Contidional to"=as.array(list(l$model$conditionalto))),
                                      list("Mean"=as.array(list(signif(ll$Mean[est][[1]],3)))),
                                      list("% Relative Bias"=as.array(list(signif(100*ll$Bias[est][[1]]/ll$E[est][[1]],3)))),
                                      list("RMSE Ratio"=as.array(list(signif(diag(as.matrix(ll$"M.S.E."[est][[1]]))/diag(as.matrix(ll$"M.S.E"["Sample"][[1]],3)))))),
                                      list("Empirical Variance"=as.array(list(signif(diag(as.matrix(ll$Variance[est][[1]],3)))))),
                                      list("Asymptotic Variance"=as.array(list(signif(diag(as.matrix(ll$V[est][[1]],3))))))))}))
    names(X)<-c("Estimator",
                "$\\theta$","$\\xi$","Conditional to","Mean","% Relative Bias","RMSE Ratio","Empirical Variance","Asymptotic Variance")
    X})}