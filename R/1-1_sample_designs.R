## definition of some sampling designs

#each sampling design returns
# - a function S of the design variable that returns a random sample
# - a vector of function Pik that depends on the design variable
# - a function demarc, that may have different meanings

#1. Simple random sampling
#param :liste param$tau : taux de sondage
SRS<-function(param){
  list(
    param=param,
    S=function(z){sample(1:length(z),size=floor(param$tau*length(z)),replace=FALSE)},
    Pik=function(z){rep(floor(param$tau*length(z))/length(z),length(z))},
    demarc=function(z){NULL})}  

#2. Stratified sampling
#param :liste param$tauh : vecteur des taux de sondage par strate
#param$proph : vecteur des taille en proportion de la pop de chaque strate.
StratS<-function(param){
  list(
    param=param,
    Pik=function(z){
      oo<-rank(z);
      N=length(z)
      Nh<-vector()
      nh<-vector()
      Pikk<-rep(NULL,length(z));cum=0
      for (i in 1:(length(param$proph)-1)){
        Nh[i]<-floor(param$proph[i]*N);
        nh[i]<-floor(param$tauh[i]*Nh[i]);
        Pikk[oo>cum&oo<=cum+Nh[i]]<-nh[i]/Nh[i];cum<-cum+Nh[i]}
      Nh[i+1]<-N-sum(Nh);nh[i+1]<-floor(param$tauh[i+1]*Nh[i+1]);Pikk[oo>cum]<-nh[i+1]/Nh[i+1];
      return(Pikk)},
    S=function(z){
      oo<-rank(z);
      N=length(z)
      Nh<-vector()
      nh<-vector()
      Sam<-vector();cum<-0
      for (i in 1:(length(param$proph)-1)){Nh[i]<-floor(param$proph[i]*N);nh[i]<-floor(param$tauh[i]*Nh[i]);
      Sam<-c(Sam,sample((1:N)[(oo>cum)&(oo<=cum+Nh[i])],size=nh[i],replace=FALSE));cum<-cum+Nh[i]}
      Nh[i+1]<-N-sum(Nh);nh[i+1]<-floor(param$tauh[i+1]*Nh[i+1]);Sam<-c(Sam,sample((1:N)[(oo>cum)&(oo<=N)],size=nh[i+1],replace=FALSE))
      return(Sam)},
    demarc=function(z){
      z<-sort(z);
      N=length(z)
      dem<-vector()
      Nh<-vector();cum<-0;
      for (i in 1:(length(param$proph)-1)){Nh[i]<-floor(param$proph[i]*N);cum<-cum+Nh[i];dem[i]<-sort(z)[cum]}
      return(dem)})}

#3. Stratified sampling 2 : z is the name of strata
#param :liste param$tauh : vecteur des taux de sondage par strate
#param$tauh : vecteur des taille en proportion de la pop de chaque strate.
StratS2<-function(param){
  list(
    param=param,
    Pik=function(z){
      ooo=sort(unique(z));
      Pikk<-rep(NULL,length(z));
      for (i in 1:(length(ooo))){
        Pikk[z==ooo[i]]<-param$tauh[i]}
      return(Pikk)},
    S=function(z){
      oo<-rank(z);
      N=length(z)
      Nh<-as.vector(table(oo))
      nh<-vector()
      Sam<-vector();cum<-0
      for (i in 1:(length(Nh))){nh[i]<-floor(param$tauh[i]*Nh[i]);
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
StratBern<-function(param){
  list(
    param=param,
    Pik=function(z){
      ooo=sort(unique(z));
      Pikk<-rep(NULL,length(z));
      for (i in 1:(length(ooo))){
        Pikk[z==ooo[i]]<-param$tauh[i]}
      return(Pikk)},
    S=function(z){
      ooo=sort(unique(z));N<-length(z);
      Pikk<-rep(NULL,length(z));
      for (i in 1:(length(ooo))){
        Pikk[z==ooo[i]]<-param$tauh[i]}
      return((1:N)[runif(N)<Pikk])},
    Sind=function(z){
      ooo=sort(unique(z));N<-length(z);
      Pikk<-rep(NULL,length(z));
      for (i in 1:(length(ooo))){
        Pikk[z==ooo[i]]<-param$tauh[i]}
      return(runif(N)<Pikk)},
    demarc=function(z){
      return(sort(unique(z)))})}
#5. Sampling with replacement and probability proportional to size
SWRPPS<-function(param){
  paramtau<-param$tau
  return(
  list(param=param,
    Pik=function(z){1-(1-z/sum(z))^(floor(paramtau*length(z)))},#Pik=function(z){z/sum(z)},
    S=function(z){sample(1:length(z),size=floor(paramtau*length(z)),prob=z/sum(z),replace=TRUE)},
    demarc=function(z){NULL}))}
#6. Cluster Sampling 
#ClusterS param=list(proph,probh,tauh)
ClusterS<-function(param){
    list(
      param=param,
      Pik=function(z){
            oo<-order(z);
            N=length(z)
            Nh<-vector()
            nh<-vector()
            Pikk<-rep(NULL,length(z));cum=0
            for (i in 1:(length(param$proph)-1)){
              Nh[i]<-floor(param$proph[i]*N);
              nh[i]<-floor(param$tauh[i]*Nh[i]);
              Pikk[oo>cum&oo<=cum+Nh[i]]<-param$proph[i]*(nh[i]/Nh[i]);
              cum<-cum+Nh[i];}
            Nh[i+1]<-N-sum(Nh);nh[i+1]<-floor(param$tauh[i+1]*Nh[i+1]);
            Pikk[oo>cum]<-param$proph[i]*(nh[i+1]/Nh[i+1]);
            return(Pikk)},
      S=function(z){
            oo<-order(z);
            N=length(z)
            nbclus<-length(param$proph)
            clus<-sample(1:nbclus,size=1,prob=param$probh,replace=FALSE);
            cum<-floor((sum(param$proph[1:clus])-param$proph[clus])*N)
            Nh<-floor(param$proph[clus]*N);
            S=sample((1:N)[oo>cum&oo<=cum+Nh],size=floor(param$tauh[clus]*Nh),replace=FALSE)
            return(S)},
        demarc=function(z){
            oo<-order(z);
            N=length(z)
            dem<-vector()
            Nh<-vector();cum<-0;
            for (i in 1:(length(param$proph)-1)){Nh[i]<-floor(param$proph[i]*N);cum<-cum+Nh[i];dem[i]<-z[oo==cum]}
            return(dem)})}
#7. Systematic sampling with selection proportional to size (and replication allowed when size larger thant step)
SystematicPPS<-function(param){
  list(param=param,
    Pik=function(z){
      N<-length(z)
      n<-floor(N*param$tau)
      return(((n*abs(z)/sum(abs(z)))>1)+((n*abs(z)/sum(abs(z)))<=1)*n*abs(z)/sum(abs(z)))},
    S=function(z){
      N<-length(z)
      n<-floor(N*param$tau)
      u<-runif(N)
      oo<-order(u)
      cum<-cumsum(abs(z)[oo])
      if(n>0){sel<-(runif(1)+(0:(n-1)))*cum[N]/n}
     return(oo[apply(outer(sel,cum,FUN=function(a,b){a<b})&outer(sel,c(0,cum[-N]),FUN=function(a,b){a>=b}),1,FUN=function(x){(1:N)[x]})])},
    demarc=function(z){NULL})}

#1. Take all sampling
#param : void liste
takeall<-function(param){
  list(
    param=param,
    S=function(z){(1:length(z))[z==1]},
    Pik=function(z){z},
    demarc=function(z){NULL})}  
