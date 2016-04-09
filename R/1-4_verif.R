source("1-1_sample_designs.R")
source("1-2_pop_model_and_sampling_design.R")
source("1-3_graphics.R")

#1. test of SRS
SRS(param=list(tau=.2))$S(1:10)
SRS(param=list(tau=.2))$Pik(1:10)
SRS(param=list(tau=.2))$demarc(1:10)
SRS(param=list(tau=.2))$param
#2. test of StratS
StratS(param=list(tauh=c(.5,.5),proph=c(.2,.8)))$S(1:20)
StratS(param=list(tauh=c(.5,.5),proph=c(.2,.8)))$Pik(1:20)
StratS(param=list(tauh=c(.5,.5),proph=c(.2,.8)))$demarc(1:20)
StratS(param=list(tauh=c(.5,.5),proph=c(.2,.8)))$param
#3. test of Strat2
StratS2(param=list(tauh=c(.5,.5,.25)))$S(c(1,1,1,1,2,2,3,3,3,3))
StratS2(param=list(tauh=c(.5,.5)))$Pik(c(1,1,1,1,2,2))
StratS2(param=list(tauh=c(.5,.5)))$demarc(c(1,1,1,1,2,2))
StratS2(param=list(tauh=c(.5,.5)))$param
#4. test of Strat2
StratBern(param=list(tauh=c(.5,.5,.25)))$S(c(1,1,1,1,2,2,3,3,3,3))
StratBern(param=list(tauh=c(.5,.5)))$Pik(c(1,1,1,1,2,2))
StratBern(param=list(tauh=c(.5,.5)))$demarc(c(1,1,1,1,2,2))
StratBern(param=list(tauh=c(.5,.5)))$param
#5. test of SWRPPS
SWRPPS(param=list(tau=.5))$S(c(1,1,1,1,2,2,3,3,3,20))
SWRPPS(param=list(tau=.5))$Pik(c(1,1,1,1,2,2))
SWRPPS(param=list(tau=.5))$demarc(c(1,1,1,1,2,2))
SWRPPS(param=list(tau=.5))$param
#6. test of ClusterS
ClusterS(param=list(proph=c(.2,.5,.3),probh=c(.1,0,.9),tauh=c(1,.5,.75)))$S(c(1,1,1,1,2,2,3,3,3,20))
ClusterS(param=list(proph=c(.2,.5,.3),probh=c(.1,0,.9),tauh=c(1,.5,.75)))$Pik(c(1,1,1,1,2,2))
ClusterS(param=list(proph=c(.2,.5,.3),probh=c(.1,0,.9),tauh=c(1,.5,.75)))$demarc(1:100)
ClusterS(param=list(proph=c(.2,.5,.3),probh=c(.1,0,.9),tauh=c(1,.5,.75)))$param
#7. test of SystematicPPS
SystematicPPS(param=list(tau=.5))$S(c(1,1,1,1,2,2,3,3,3,4))
SystematicPPS(param=list(tau=.5))$Pik(c(1,1,1,1,2,2))
SystematicPPS(param=list(tau=.5))$demarc(c(1,1,1,1,2,2))
SystematicPPS(param=list(tau=.5))$param

#2. test of population models and sampling designs.

#2.1 test of calculeV

calculeV(1,1,1,1,1)
A=diag(c(1,2,3));B=diag(c(1,2));calculeV(A,A,A,A,A)
rm(A);rm(B)

#2.2 Test of model.Pareto.bernstrat
m<-model.Pareto.bernstrat(sampleparam=list(tauh=c(.02,.1)),theta=.5,xi=2,param=list())
mean(m$rloiy(100));4/(4-1)
      plot((0:3000)/100,m$ploi((0:3000)/100),type='l')
      lines((0:3000)/100,m$ploilim((0:3000)/100),col='red')
      m$ploilim=function(y){pgamma(y,3/2,2)}
      rloiz=function(y){rbinom(length(y),size=1,prob=1/y^xi)},
      dloi=function(y){theta/(y^(theta+1))}
      dloitheta=function(y,theta){theta/(y^(theta+1))},
      Scheme=StratBern(sampleparam),
      calculsintermediairespourjac=function(y){},
      Jacobiane=function(listeqtes,theta,xi,n){},
      hessiane=function(listeqtes,theta,xi,n){},
      rhothetaxi=function(y,theta,xi){  as.vector(((sampleparam$tauh%*%rbind(1-theta/(xi+theta),theta/(xi+theta)))^{-1})[1,1]*
                                                    (sampleparam$tauh%*%rbind(1-1/(y^xi), 1/(y^xi))))},
      rho=function(y){ as.vector(((sampleparam$tauh%*%rbind(xi/(xi+theta),theta/(xi+theta)))^{-1})[1,1]*
                                   (sampleparam$tauh%*%rbind(1-1/(y^xi),1/(y^xi))))},
      sampleparam=sampleparam,
      tau=tau,
      #logl1prime=logl1prime,
      I11MC=function(){} ,
      I12MC=function(){},
      I11formula=I11formula,
      I12formula=I12formula,
      I11=I11formula,
      I12=I12formula,
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
      Sigma=calculeSigma(),
      V=Sigma[1,1]/I11formula^2+I12formula/I11formula^2*(Sigma[2,2]*I12formula-2*Sigma[1,2]),
      VHT=tau*(
        ((tauh[1]^(-1)-1)^2*tauh[1]+(1-tauh[1]))*(-(2-theta)^(-1)*theta+(2-xi-theta)^(-1)*theta)
        +(tauh[2]^(-1)-1)^2*tauh[2]+(1-tauh[2])*(xi+theta-2)^(-1)*theta
        +(-(2-theta)^(-1)*theta-(1-theta)^(-2)*theta^(2)))*(theta-1)^(4),
      Vniais=NULL,
      supportY=c(-.1,2.1)))
}


#2.3. test of model.dep.strat

popmodelfunction<-model.dep.strat;proph=c(.7,.3);tauh=c(1/70,2/15);sigma=10
sampleparam<-list(proph=proph,tauh=tauh);
Param<-list(list(sigma=0.1),list(sigma=1),list(sigma=10));
N<-5000;nbreps<-400;
theta=1.5;xi=2;param=list(sigma=sigma);
m<-popmodelfunction(sampleparam,theta,xi,param)
plotpdf(popmodelfunction,sampleparam,N,theta,xi,param)
#2.3.1 test of I11
m$I11                  
G<-genere(m,100000);yg=G$Yg[G$Sg];ll1<-m$deriveetheta(yg,sampleparam,theta,xi,param)

#this should have mean 0
mean(ll1)
#this should be equal to I11
mean(ll1^2)    
#this should be equal to I11
var(ll1)
#this should be equal to I11
mean(m$deriveetheta(G$Yg,sampleparam,theta,xi,param)^2*m$rho(G$Yg))
1/(m$I11*m$tau*5000)                  
m$V/(m$tau*5000)
