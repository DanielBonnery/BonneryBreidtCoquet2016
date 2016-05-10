## definition of some sampling designs

#each sampling design returns
# - a function S of the design variable that returns a random sample
# - a vector of function Pik that depends on the design variable
# - a function demarc, that may have different meanings

#1. Simple random sampling
#sampleparam :liste sampleparam$tau : taux de sondage
SRS<-function(sampleparam){
  list(
    sampleparam=sampleparam,
    S=function(z){sample(1:length(z),size=floor(sampleparam$tau*length(z)),replace=FALSE)},
    Pik=function(z){rep(floor(sampleparam$tau*length(z))/length(z),length(z))},
    demarc=function(z){NULL})}  

#2. Stratified sampling
#sampleparam :liste sampleparam$tauh : vecteur des taux de sondage par strate
#sampleparam$proph : vecteur des taille en proportion de la pop de chaque strate.
StratS<-function(sampleparam){
  list(
    sampleparam=sampleparam,
    Pik=function(z){
      N=length(z)
      Nh<-floor(sampleparam$proph*N);Nh[length(Nh)]=Nh[length(Nh)]+N-sum(Nh)
      nh<-floor(sampleparam$tauh*Nh);
      Pikk<-rep(nh/Nh,Nh)[rank(z)]
      return(Pikk)},
    S=function(z){
      N=length(z)
      Nh<-floor(sampleparam$proph*N);Nh[length(Nh)]=Nh[length(Nh)]+N-sum(Nh)
      nh<-floor(sampleparam$tauh*Nh);
      unlist(sapply(1:length(Nh),function(i){
        sample((1:N)[(rank(z)>c(0,cumsum(Nh))[i])&(rank(z)<=cumsum(Nh)[i])],nh[i])}))
      },
    demarc=function(z){
      N=length(z)
      Nh<-floor(sampleparam$proph*N);Nh[length(Nh)]=Nh[length(Nh)]+N-sum(Nh)
      z[order(z)][cumsum(Nh)[-length(Nh)]]
      })}

#3. Stratified sampling 2 : z is the name of strata
#sampleparam :liste sampleparam$tauh : vecteur des taux de sondage par strate
#sampleparam$tauh : vecteur des taille en proportion de la pop de chaque strate.
StratS2<-function(sampleparam){
  list(
    sampleparam=sampleparam,
    Pik=function(z){
      ooo=sort(unique(z));
      Pikk<-rep(NULL,length(z));
      for (i in 1:(length(ooo))){
        Pikk[z==ooo[i]]<-sampleparam$tauh[i]}
      return(Pikk)},
    S=function(z){
      oo<-rank(z);
      N=length(z)
      Nh<-as.vector(table(oo))
      nh<-vector()
      Sam<-vector();cum<-0
      for (i in 1:(length(Nh))){nh[i]<-floor(sampleparam$tauh[i]*Nh[i]);
      Sam<-c(Sam,sample((1:N)[(oo>cum)&(oo<=cum+Nh[i])],size=nh[i],replace=FALSE));cum<-cum+Nh[i]}
      return(Sam)},
    demarc=function(z){
      oo<-rank(z);
      z<-sort(z);
      N=length(z)
      Nh<-as.vector(table(oo))
      dem<-vector()
      Nh<-vector();cum<-0;
      for (i in 1:(length(Nh)-1)){cum<-cum+Nh[i];dem[i]<-sort(z)[cum]}
      return(dem)})}
#4. Stratifi? avec Bernoulli dans chaque strate
StratBern<-function(sampleparam){
  list(
    sampleparam=sampleparam,
    Pik=function(z){
      ooo=sort(unique(z));
      Pikk<-rep(NULL,length(z));
      for (i in 1:(length(ooo))){
        Pikk[z==ooo[i]]<-sampleparam$tauh[i]}
      return(Pikk)},
    S=function(z){
      ooo=sort(unique(z));N<-length(z);
      Pikk<-rep(NULL,length(z));
      for (i in 1:(length(ooo))){
        Pikk[z==ooo[i]]<-sampleparam$tauh[i]}
      return((1:N)[runif(N)<Pikk])},
    Sind=function(z){
      ooo=sort(unique(z));N<-length(z);
      Pikk<-rep(NULL,length(z));
      for (i in 1:(length(ooo))){
        Pikk[z==ooo[i]]<-sampleparam$tauh[i]}
      return(runif(N)<Pikk)},
    demarc=function(z){
      return(sort(unique(z)))})}
#5. Sampling with replacement and probability proportional to size
SWRPPS<-function(sampleparam){
  sampleparamtau<-sampleparam$tau
  return(
  list(sampleparam=sampleparam,
    Pik=function(z){1-(1-z/sum(z))^(floor(sampleparamtau*length(z)))},#Pik=function(z){z/sum(z)},
    S=function(z){sample(1:length(z),size=floor(sampleparamtau*length(z)),prob=z/sum(z),replace=TRUE)},
    demarc=function(z){NULL}))}
#6. Cluster Sampling 
#ClusterS sampleparam=list(proph,probh,tauh)
ClusterS<-function(sampleparam){
    list(
      sampleparam=sampleparam,
      Pik=function(z){
            oo<-order(z);
            N=length(z)
            Nh<-vector()
            nh<-vector()
            Pikk<-rep(NULL,length(z));cum=0
            for (i in 1:(length(sampleparam$proph)-1)){
              Nh[i]<-floor(sampleparam$proph[i]*N);
              nh[i]<-floor(sampleparam$tauh[i]*Nh[i]);
              Pikk[oo>cum&oo<=cum+Nh[i]]<-sampleparam$proph[i]*(nh[i]/Nh[i]);
              cum<-cum+Nh[i];}
            Nh[i+1]<-N-sum(Nh);nh[i+1]<-floor(sampleparam$tauh[i+1]*Nh[i+1]);
            Pikk[oo>cum]<-sampleparam$proph[i]*(nh[i+1]/Nh[i+1]);
            return(Pikk)},
      S=function(z){
            oo<-order(z);
            N=length(z)
            nbclus<-length(sampleparam$proph)
            clus<-sample(1:nbclus,size=1,prob=sampleparam$probh,replace=FALSE);
            cum<-floor((sum(sampleparam$proph[1:clus])-sampleparam$proph[clus])*N)
            Nh<-floor(sampleparam$proph[clus]*N);
            S=sample((1:N)[oo>cum&oo<=cum+Nh],size=floor(sampleparam$tauh[clus]*Nh),replace=FALSE)
            return(S)},
        demarc=function(z){
            oo<-order(z);
            N=length(z)
            dem<-vector()
            Nh<-vector();cum<-0;
            for (i in 1:(length(sampleparam$proph)-1)){Nh[i]<-floor(sampleparam$proph[i]*N);cum<-cum+Nh[i];dem[i]<-z[oo==cum]}
            return(dem)})}
#7. Systematic sampling with selection proportional to size (and replication allowed when size larger thant step)
SystematicPPS<-function(sampleparam){
  list(sampleparam=sampleparam,
    Pik=function(z){
      N<-length(z)
      n<-floor(N*sampleparam$tau)
      return(((n*abs(z)/sum(abs(z)))>1)+((n*abs(z)/sum(abs(z)))<=1)*n*abs(z)/sum(abs(z)))},
    S=function(z){
      N<-length(z)
      n<-floor(N*sampleparam$tau)
      u<-runif(N)
      oo<-order(u)
      cum<-cumsum(abs(z)[oo])
      if(n>0){sel<-(runif(1)+(0:(n-1)))*cum[N]/n}
     return(oo[apply(outer(sel,cum,FUN=function(a,b){a<b})&outer(sel,c(0,cum[-N]),FUN=function(a,b){a>=b}),1,FUN=function(x){(1:N)[x]})])},
    demarc=function(z){NULL})}

#1. Take all sampling
#sampleparam : void liste
takeall<-function(sampleparam=NULL){
  list(
    sampleparam=sampleparam,
    S=function(z){(1:length(z))[z==1]},
    Pik=function(z){z},
    demarc=function(z){NULL})}  
