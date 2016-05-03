#test

popmodelfunction<-model.Pareto.bernstrat;
sampleparam<-list(tauh=c(0.01,0.1));N<-10000;theta<-4;xi=.1;
param<-list();nbreps<-400;method="nlm";
nbreps=3

model<-popmodelfunction(sampleparam,theta,xi,param)

testcav=FALSE
testsim=TRUE
testfullMLE=FALSE
#test cav:
if(testcav){Sigma <- calcule.Sigma(model,N,nbrepSigma)
Im <- Imatrix9(N,model)
dimtheta<-length(model$theta);
V123<-calculeV(Sigma,Im,dimtheta)
V <- V123$V/(N*model$tau)
VHT<-NA;#(sum(tauh*proph))*sum(tauh*proph*Vh.ypi_1)
Vniais<-NA;#1/(sum(tauh*proph))*sum(tauh*proph*Vh.y)
cave=list(Sigma=Sigma,Im=Im,VHT=VHT,V=V,Vniais=Vniais,
            V1=V123$V1/(N*model$tau),V2=V123$V2/(N*model$tau),V3=V123$V3/(N*model$tau))}else{cave <- cav(model,N,nbrepSigma=1000,nbrepI=3000)}
#testsim
if(testsim){
  attach(model)
  attach(Scheme)
  #initialization : those vectors will contain the values of the 
  Xg<-rloix(N);
  Estim <- plyr::rlply(2,
                       (function(){
                         #Population generation and sample selection
                         Yg<-cbind(Xg,rloiy.x(X,N));  #Y generation
                         Zg<-rloiz(Yg); #Z generation
                         Sg<-S(Zg);     #sample selection
                         Pikg<-Pik(Zg)
                         
                         xi.hat   =xihat     (Yg,Zg,Sg,Pikg);
                         theta.ht =thetaht   (Yg,Zg,Sg,Pikg);
                         theta.bar=thetaniais(Yg,Zg,Sg);
                         if(!testfullMLE){thetaxi.full=fullMLE(Yg,Zg,Sg,model)}else{
                           y=Yg;z=Zg;s=Sg
                           if(is.vector(y)){ys<-y[s]}
                           if(is.matrix(y)){ys<-y[s,]}
                           if(!is.null(model$fulllikelihood)){log(model$fulllikelihood(y,thetaxi))}else{}
                           optimx::optimx(c(model$theta,model$xi),
                                          fn =full.likelihood,control=list(maximize=TRUE,method="nlm"),model=model,y=ys,z=z)}
                         
                         theta.hat=sampleMLE(Yg,Zg,Sg,model,method)
                         list(xi.hat   =xi.hat,
                                     theta.ht =theta.ht,
                                     theta.bar=theta.bar,
                                     thetaxi.full=fullMLE(Yg,Zg,Sg,model),
                                     theta.hat=sampleMLE(Yg,Zg,Sg,model,method))})())
  
  noms<-names(Estim[[1]])
  Estim<-lapply(as.list(noms),function(nom){plyr::laply(Estim,function(ll){ll[[nom]]})})
  names(Estim)<-noms
  attach(Estim)
  Var<-lapply(Estim,var)
  M=lapply(Estim,function(est){apply(as.matrix(est),2,mean)})
  E=list(model$xi,model$theta,model$theta,model$theta,model$theta)
  names(E)<-names(M)
  Bias=lapply(as.list(noms),function(x){M[[x]]-E[[x]]})
  names(Bias)<-names(M)
  MSE=lapply(as.list(noms),function(x){Var[[x]]+Bias[[x]]%*%t(Bias[[x]])})
  names(MSE)<-names(M)
  sim<-list(
    model=model,
    Estim=Estim,
    Var=Var,
    M=M,
    Bias=Bias,
    MSE=MSE)}else{sim<-simule(N=N,model,method,nbreps=2)}

simulation_data<-Simulation_data(nbreps,popmodelfunction,sampleparam,N,theta,xi,param,method)
