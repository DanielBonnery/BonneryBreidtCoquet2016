model.dep.strat<-function(sampleparam,theta,xi,param){
    #for simplicity of notation
  proph<-sampleparam$proph
  tauh<-sampleparam$tauh
  sigma<-param$sigma
    Scheme=StratS(sampleparam)

    deriveetheta1<-function(y,sampleparam,theta,xi,param){
       tauh<-sampleparam$tauh;
       proph<-sampleparam$proph;
       sigma<-param$sigma;
        H<-length(tauh);     deno<-tauh[H];nume<-0
        t<-proph[1]
        for(h in (1:(H-1))){
          deno<-deno+(tauh[h]-tauh[h+1])           *pnorm(sqrt((xi/sigma)^2+1)*qnorm(t)+(xi/sigma)*(theta-y),mean=0,sd=1);
          nume<-nume+(tauh[h]-tauh[h+1])*(xi/sigma)*dnorm(sqrt((xi/sigma)^2+1)*qnorm(t)+(xi/sigma)*(theta-y),mean=0,sd=1)
          t<-t+proph[h+1]}
          return((y-theta)+(nume/deno))}
    deriveetheta<-function(y,sampleparam,theta,xi,param){
          return(sapply(y,deriveetheta1,sampleparam=sampleparam,theta=theta,xi=xi,param=param))}
    deriveexi1<-function(y,sampleparam,theta,xi,param){
       tauh<-sampleparam$tauh;
       proph<-sampleparam$proph;
       sigma<-param$sigma;
        H<-length(tauh);     deno<-tauh[H];nume<-0
        t=proph[1]
        for(h in (1:(H-1))){
          deno<-deno+(tauh[h]-tauh[h+1])*pnorm((sqrt(xi^2+sigma^2)*qnorm(t)+xi*(theta-y))/sigma,mean=0,sd=1);
          nume<-nume+(tauh[h]-tauh[h+1])*(((xi/sigma^2)*((xi^2/sigma^2+1)^(-1/2))*qnorm(t)+(theta-y))/sigma)*dnorm((sqrt(xi^2+sigma^2)*qnorm(t)+xi*(theta-y))/sigma,mean=0,sd=1)
          t<-t+proph[h+1]}
          return((nume/deno))}
    deriveexi<-function(y,sampleparam,theta,xi,param){
          return(sapply(y,deriveexi1,sampleparam=sampleparam,theta=theta,xi=xi,param=param))}
    Ninf=100000;Nhinf=floor(Ninf*proph);
    yinf<-rnorm(Ninf)+2;
    sinf<-StratS(sampleparam)$S(rnorm(yinf,mean=xi*yinf,sd=sigma));
    tau=sum(proph*tauh)
    rho=function(y){
        rhorho<-tauh[length(tauh)];t<-proph[1]
      for(h in 1:(length(tauh)-1)){rhorho<-rhorho+(tauh[h]-tauh[h+1])*pnorm((sqrt(xi^2+sigma^2)*qnorm(t)+xi*(theta-y))/sigma);t<-t+proph[h+1]}
      return(rhorho/tau)}
    rhothetaxi=function(y,theta,xi){
        rhorho<-tauh[length(tauh)];t<-proph[1]
      for(h in 1:(length(tauh)-1)){rhorho<-rhorho+(tauh[h]-tauh[h+1])*pnorm((sqrt(xi^2+sigma^2)*qnorm(t)+xi*(theta-y))/sigma);t<-t+proph[h+1]}
      return(rhorho/tau)}

  cav<-function(sampleparam,theta,xi,param,nrep){
        y=rnorm(nrep,mean=theta,sd=1);
        dd= deriveetheta(y,sampleparam=sampleparam,theta=theta,xi=xi,param=param)
        ddxi= deriveexi(y,sampleparam=sampleparam,theta=theta,xi=xi,param=param)
        rhorho=rhothetaxi(y,theta,xi)
        I11=mean(dd^2*rhorho)
        I12=mean(dd*ddxi*rhorho)
   
       tauh<-sampleparam$tauh;
       proph<-sampleparam$proph;
       sigma<-param$sigma;
        tau<-sum(tauh*proph)
        eta<-rnorm(nrep,mean=0,sd=sigma);
        z<-xi*y+eta      
        #new verion
        H=length(tauh);th=vector()
        Bh=vector();th[1]=proph[1];
        for(h in 2:H){th[h]=proph[h-1]+proph[h]}
        Bh=xi*theta+sqrt(xi^2+sigma^2)*qnorm(th)
        toto<-function(z,Bh){(z>c(-Inf,Bh[-H]))&(z<=Bh)}
        ind<-t(sapply(z,toto,Bh=Bh));
        Sigma<-0;
        for(h in 1:H){
          Sigma<-Sigma+tauh[h]*proph[h]/tau*
                  (var(cbind(dd[ind[,h]],
                             y[ind[,h]]*z[ind[,h]]/tauh[h],
                             y[ind[,h]]^2/tauh[h])))}
        #old version
        H=length(tauh);th=vector()
        Bh=vector();th[1]=proph[1];
        for(h in 2:H){th[h]=proph[h-1]+proph[h]}
        Bh=xi*theta+sqrt(xi^2+sigma^2)*qnorm(th)
        toto<-function(z,Bh){(z>c(-Inf,Bh[-H]))*(z<=Bh)}
        ind<-sapply(z,toto,Bh=Bh);
        ind[ind==0]<-NA
        w<-ind/tauh
        ind<-t(ind)  ;w<-t(w)        
        Vh.ypi_1<-apply(y*w,2,var,na.rm=TRUE)
        Vh.y<-apply(y*ind,2,var,na.rm=TRUE)
        Vh.y2pi_1<-apply(y^2*w,2,var,na.rm=TRUE)
        Vh.yzpi_1<-apply(y*z*w,2,var,na.rm=TRUE)
        Vh.delta<-apply(dd*ind,2,var,na.rm=TRUE)
        Covh.y2pi_1yzpi_1<-apply((y^3*z)*w^2,2,mean,na.rm=TRUE)-apply(y^2*w,2,mean,na.rm=TRUE)*apply(y*z*w,2,mean,na.rm=TRUE)
        Covh.y2pi_1delta<-apply(y^2*dd*w,2,mean,na.rm=TRUE)-apply(y^2*w,2,mean,na.rm=TRUE)*apply(dd*ind,2,mean,na.rm=TRUE)
        Covh.yzpi_1delta<-apply(y*z*dd*w,2,mean,na.rm=TRUE)-apply(y*z*w,2,mean,na.rm=TRUE)*apply(dd*ind,2,mean,na.rm=TRUE)
            Sigmah<-list();
        for(h in 1:H){
          Sigmah[[h]]<-
            matrix(c(Vh.delta[h],Covh.yzpi_1delta[h],Covh.y2pi_1delta[h],Covh.yzpi_1delta[h],Vh.yzpi_1[h],Covh.y2pi_1yzpi_1[h],Covh.y2pi_1delta[h],Covh.y2pi_1yzpi_1[h],Vh.y2pi_1[h]),3,3)}
       if(FALSE){
           MA<-matrix(c(1,0,0,0,1/(theta^2+1),-xi/(theta^2+1)),3,2)
        Sigma<-0 ;
        for(i in (1:H)){
          Sigma<-tauh[h]*Sigmah[[h]]*proph[h]+Sigma}}
       MA<-matrix(c(1,0,0,0,tau/(theta^2+1),-tau*xi/(theta^2+1)),3,2)
        Sigma<-t(MA)%*%Sigma%*%MA
        V<-calculeV(Sigma,matrix(c(I11,I12,I12,NA),2,2),1)
        V1<-calculeV(matrix(c(Sigma[1,1],0,0,0),c(2,2)),matrix(c(I11,I12,I12,NA),2,2),1)
          VHT<-(sum(tauh*proph))*sum(tauh*proph*Vh.ypi_1)
          Vniais<-1/(sum(tauh*proph))*sum(tauh*proph*Vh.y)
         return(list(Sigma=Sigma,Sigmah=Sigmah,MA=MA,I11=I11,I12=I12,VHT=VHT,V=V,V1=V1,Vniais=Vniais))}
    cave=cav(sampleparam,theta,xi,param,10000);

     cav2<-function(N){
        y<-rloiy(N*20)
        dd=deriveetheta(y,sampleparam=sampleparam,theta=theta,xi=xi,param=param)
        ddxi= deriveexi(y,sampleparam=sampleparam,theta=theta,xi=xi,param=param)
        rhorho=rhothetaxi(y,theta,xi)
        I11=t(dd)%*%(dd*rhorho)/nrep
        I12=t(dd)%*%(ddxi*rhorho)/nrep
        Sigma1<-
          var(t(sapply(1:10000,
               function(qcq){
                 y=rloiy(N)
                 z=rloiz(y)
                 s <- Scheme$S(z);
                 pi=Scheme$Pik(z);                 
                 return(apply(cbind(deriveetheta(cbind(x[s],y[s]),sampleparam=sampleparam,theta=theta,xi=xi,param=param),y[s]*z[s]/pi[s],y[s]^2/pi[s]),2,sum))})))
     
       MA<-matrix(c(1,0,0,0,tau/(theta^2+1),-tau*xi/(theta^2+1)),3,2)
        Sigma<-(1/(N*tau))*t(MA)%*%Sigma1%*%MA;
        Sigma11<-Sigma[1,1];Sigma22<-Sigma[2,2];Sigma12<-Sigma[1,2]
        V<-calculeV(Sigma11,Sigma12,Sigma22,I11,I12)
        V1<-NA;#calculeV(Sigma11,0,0,I11,0)
        VHT<-NA;#(sum(tauh*proph))*sum(tauh*proph*Vh.ypi_1)
        Vniais<-NA;#1/(sum(tauh*proph))*sum(tauh*proph*Vh.y)
         return(list(Sigma=Sigma,Sigma1=Sigma1,I11=I11,I12=I12,VHT=VHT,V=V,V1=V1,Vniais=Vniais))}
    #cave2=cav2(5000);


    
    rloiy=function(N){rnorm(N,theta,1)}

  return(list(
    sampleparam=sampleparam,
    theta=theta,xi=xi,param=param,
    rloix=function(N){NULL},
    rloiy.x=function(x,N){rloiy(N)},
    rloiy=rloiy,
    ploi=function(y){pnorm(y,theta,1)},
    dloi=function(y){dnorm(y,theta,1)},
    dloitheta=function(y,theta){dnorm(y,theta,1)},
    rloiz=function(y){rnorm(y,mean=xi*y,sd=sigma)},
    dloilim=function(y){return(rho(y)*dnorm(y,theta,1))},
    deriveetheta=deriveetheta,
    ploilim=function(y){
      yy<-rnorm(1000,theta,1);
      eps<-rnorm(10000,0,sigma);
      toto<-0
      for(h in 1:length(tauh)){
        limites<-sqrt(xi^2+sigma^2)*qnorm(cumsum(c(0,proph)))+xi*theta
        toto<-toto+proph[h]*tauh[h]*ecdf(yy[xi*yy+eps<=limites[h+1]&xi*yy+eps>limites[h]])(y)
        }
      return(toto/(sum(proph*tauh)))}, #no formal expression
    Scheme=StratS(sampleparam),
    rho=rho,
    rhothetaxi=rhothetaxi,
    vinf=function(y){tau*rho(y)-(tau*rho(y))^2},
    En=function(N){tau*N},
    tau=tau,
    I11=cave$I11,
    I12=cave$I12,
    V=cave$V,
    VHT=cave$VHT,
    Vniais=cave$Vniais,
    Sigma=cave$Sigma,
    xihat=function(y,z,s,pik){ #inclusion probabilities
      s.zy<-sum((z*y/pik)[s]) #HT estimator of $\sum_{k=1}^N Y_k Z_k$
      s.y2<-sum((y^2/pik)[s])  #HT estimator of $\sum_{k=1}^N Y_k^2$
      xi.hat<-(s.zy)/(s.y2)    #estimator of $xi$}
      return(xi.hat)},
    xihatfunc1 =function(y,z,pik){cbind(z*y/pik,y^2/pik)},
    xihatfunc2 =function(u){u[1]/u[2]},
    xihatfuncdim=2,
    rhoxthetaxi=function(x,theta,xi){1},
    thetaht=function(y,z,s,pik){
      s.y<-sum((y/pik)[s])     #HT estimator of $\sum_{k=1}^N Y_k$
      s.1<-sum((1/pik)[s])      #HT estimator of $N$
      theta.ht<-s.y/s.1
      return(theta.ht)},
    thetaniais=function(y,z,s){
      return(mean(y[s]))},
    thetahat=function(y,z,s){},
    supportY=c(-.5,5)))}
