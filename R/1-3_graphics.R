source("1-1_sample_designs.R")
source("1-2_pop_model_and_sampling_design.R")


###1. plot of sample theoretical and empirical cdf, population theoretical cdf.
plotcdf<-function(popmodelfunction,sampleparam,N,theta,xi,param){
  m<-popmodelfunction(sampleparam,theta,xi,param)
  G<-genere(m,N)
  plot(ecdf(G$Yg),col="black")
  lines(ecdf(G$Yg[G$Sg]),col="orange")
  x<-min(G$Yg)+seq(0,1,.001)*(max(G$Yg)-min(G$Yg))
  lines(x,m$ploilim(x),col="blue") 
  }

plotpdf<-function(popmodelfunction,sampleparam,N,theta,xi,param){
  m<-popmodelfunction(sampleparam,theta,xi,param)
  G<-genere(m,10000)
  x<-min(G$Yg)+seq(0,1,.01)*(max(G$Yg)-min(G$Yg))
  plot(x,m$dloilim(x),col="blue",type='l') 
  lines(x,m$dloi(x),col="black")
  lines(density(G$Yg[G$Sg]),col="orange")
  }



###2. Vieux code
##1.6. Code tex pour graphiques 
##1.6.1. fonctions génériques de tracé.
#ouverture de fichier
debutfic<-function(fic,echellegraph=c(1,1),limites=c(-0.35,-0.13,1.1,1.1)){
    write(paste("\\begin{tikzpicture}[line cap=round,line join=round,>=triangle 45,x=",echellegraph[1],"cm,y=",echellegraph[2],"cm]"),file=fic,append=F)
    write(paste("\\clip(",limites[1],",",limites[2],") rectangle (",limites[3],",",limites[4],");"),file=fic,append=T)}
#tracé des axes
traceaxes<-function(fic,limites=c(-0.35,-0.13,1.1,1.1),width=1.2){
    write("%%Les axes",file=fic,append=T)
    styleaxe="->";aj="";if (styleaxe=="->"){aj<-"->,"}
    write(paste("\\draw[",aj,"line width=",width,"pt,color=black] (",limites[1],",0) --  (",limites[3],",0);"),file=fic,append=T)
    write(paste("\\draw[",aj,"line width=",width,"pt,color=black] (0,",limites[2],") -- (0,",limites[4],");"),file=fic,append=T)}
#tracé de courbes
tracecourbe<-function(fic,x,Y,couleurcourbe=c("black"),width=1.5){
    write("%Les courbes",file=fic,append=T)
    for(i in 1:length(Y[1,])){xc<-x[!is.na(x)&!is.na(Y[,i])];yc<-(Y[,i])[!is.na(x)&!is.na(Y[,i])];
    if (sum(!is.na(yc))>2){lb<-(max(length(yc),length(xc))-1)
    write(paste("%%courbe ",i),file=fic,append=T)
    for (j in 1:lb){
      write(paste("\\draw[line width=",width,"pt,color=",couleurcourbe[i],
                       "] (",  formatC(xc[j],    format="f"),",",formatC(yc[j],  format="f"),
                       ")-- (",formatC(xc[j+1],format="f"),",",formatC(yc[j+1],format="f"),");"),file=fic,append=T)}}}
}
#tracé d'une fonction de répartition empirique 
tracefdr<-function(fic,couleur="black",y,limites,width=2){
    write("%FDR",file=fic,append=T)
    nn=length(y)
    ys=c(limites[1],sort(y),limites[2]);
    for(i in (1:(nn+1))){
      if(ys[i]!=ys[i+1]){ 
    if (i < nn+1){
    #  write(paste("\\draw [color=",couleur,"] (",ys[i+1],",",(i-1)/nn,") circle (3pt);"),file=fic,append=T)
        }}
    if (i >1){
     # if (ys[i]<ys[i+1]){write(paste("\\fill [color=",couleur,"] (",ys[i],",",(i-1)/nn,") circle (3pt);"),file=fic,append=T)}
    }}
    for(i in (1:(nn+1))){
      if(ys[i]!=ys[i+1]){ 
    write(paste("\\draw [line width=",width,"pt,color=",couleur,"] (",formatC(ys[i],format="f"),",",formatC((i-1)/nn,format="f"),") -- (",formatC(ys[i+1],format="f"),",",formatC((i-1)/nn,format="f"),");"),file=fic,append=T)}}}
#tracé de graduations sur les axes
tracegrades<-function(fic,repx="",repy=""){
    write("%grades",file=fic,append=T)
      xp=paste(repx,collapse=",");
      if(xp!=""){
    write(paste("\\foreach \\x in {",xp,"}"),file=fic,append=T)
    write(paste("\\draw[shift={(\\x,0)},color=black] (0pt,2pt) -- (0pt,-2pt) node[below] {\\footnotesize $\\x$};"),file=fic,append=T)}
      yp=paste(repy,collapse=",");
      if(yp!=""){
    write(paste("\\foreach \\y in {",yp,"}"),file=fic,append=T)
    write(paste("\\draw[shift={(0,\\y)},color=black] (2pt,0pt) -- (-2pt,0pt) node[left] {\\footnotesize $\\y$};"),file=fic,append=T)}}
# écriture de texte sur le graphique
tracetexte<-function(fic,texte=c("$Z_{\\gamma k}$","$Y_k$"),posy=posy,posz=posz){
    write("%texte",file=fic,append=T)
    for (i in 1:length(texte)){write(paste("\\draw[color=black] (",posy[i],",",posz[i],") node {",texte[i],"};"),file=fic,append=T)}}
# tracé de points
tracepts<-function(fic,y,z,color="black",diam=1){
    write("%points",file=fic,append=T)
      for(i in 1:length(y)){write(paste("\\fill[color=",color,"] (",formatC(y[i],format="f"),",",formatC(z[i],format="f"),") circle (",diam,"pt);"),file=fic,append=T)}}
# tracé de points
traceptsrep<-function(fic,y,z,s,couleurpoints="gray",diam=3){
    write("%points selectionnés",file=fic,append=T)
      for(k in unique(s)){
    ik<-sum(s==k)
    if(ik>1){
    write(paste("\\fill[color=",couleurpoints,"] (",formatC(y[k],format="f"),",",formatC(z[k],format="f"),") circle (",diam,"pt) node[below] {\\footnotesize $",sum(s==k),"$};"),file=fic,append=T)}
    if(ik==1){
    write(paste("\\fill[color=",couleurpoints,"] (",formatC(y[k],format="f"),",",formatC(z[k],format="f"),") circle (",diam,"pt);"),file=fic,append=T)}
    }}
# tracé de lignes en pointillés
tracepointillesh<-function(fic,demarc,color="gray",limites,width=2.4){
  if (!is.null(demarc)){
    write("%lignes",file=fic,append=T)
    for(demarq in demarc){
    write(paste("\\draw[line width=",width,"pt,dash pattern=on 2pt off 2pt,color=gray] (0,",demarq,") -- (",limites[3],",",demarq,");%axe de separation des strates"),file=fic,append=T)}}}#separation des strates
tracepointillesv<-function(fic,demarc,color="gray",limites,width=2.4){
  if (!is.null(demarc)){
    write("%lignes",file=fic,append=T)
    write(paste("\\draw[line width=",width,"pt,dash pattern=on 2pt off 2pt,color=gray] (",demarc,",0) -- (",demarc,",",limites[4],");%axe de separation des strates"),file=fic,append=T)}}#separation des strates
# génération de code pour un graphique
generecodetexgraph<-function(fic,y,s,z,taillegraph,prop,quoi,x,Y,repx=repx,repy=repy,leg,couleurs=couleurs,demarc=NULL,tailles=tailles,maxy,maxz){
        limites=prop*c(maxy,maxz,maxy,maxz)
        echellegraph<-taillegraph/c(limites[3]-limites[1],limites[4]-limites[2])
        leg$posy<-leg$posy*maxy
        leg$posz<-leg$posz*maxz
        if (max(quoi=="deb")){debutfic(fic,echellegraph=echellegraph,limites=limites)}
        if (max(quoi=="axes")){traceaxes(fic,limites=limites,width=tailles$axe)}
        if (max(quoi=="courbe")){tracecourbe(fic,x,Y,couleurcourbe=couleurs$courbe,width=tailles$courbe)}
        if (max(quoi=="fdr")){tracefdr(fic,couleur=couleurs$cdf,y,limites[c(1,3)],width=tailles$cdf)}
        if (max(quoi=="fdremp")){tracefdr(fic,couleur=couleurs$scdf,y[s],limites[c(1,3)],width=tailles$scdf)}
        if (max(quoi=="grades")){tracegrades(fic,repx=repx,repy=repy)}
        if (max(quoi=="ppts")){tracepts(fic,y,z,color=couleurs$pts,diam=tailles$pts)}
        if (max(quoi=="spts")){traceptsrep(fic,y,z,s,couleurpoints=couleurs$ptss,diam=tailles$ptss)}
        if (max(quoi=="texte")){tracetexte(fic,texte=leg$texte,posy=leg$posy,posz=leg$posz)}
        if (max(quoi=="lignev")){tracepointillesv(fic,demarc,color=couleurs$ptl,limites,width=tailles$ptl)}
        if (max(quoi=="ligneh")){tracepointillesh(fic,demarc,color=couleurs$ptl,limites,width=tailles$ptl)}
        if (max(quoi=="strate")){
              write("\\usefont{T1}{ptm}{m}{n}",file=fic,append=T);
              write("\\rput{-270.0}(-0.05,0.2){\\rput(0.1,0){strata 1}};",file=fic,append=T)
              write("\\usefont{T1}{ptm}{m}{n}",file=fic,append=T);
              write("\\rput{-270.0}(-0.05,0.6){\\rput(0.1,0){strata 2}};",file=fic,append=T)}
        if (max(quoi=="fin")){write("\\end{tikzpicture}",file=fic,append=T)}}
##1.6.2 Graphic generation function 
#default parameters
legende<-list(
    list(posy=c(0.09,0.95),posz=c(1,0.1),texte=c("$Z_{k}$","$Y_k$"),col=rep("black",2)),
    list(posy=c(0.12,0.45),posz=c(0.8,0.55,0.8,0.55,.1),texte=c("$F_{\\infty}$","$Y_k$"),col=rep("black",2)),
    list(posy=c(0.12,0.45),posz=c(0.8,0.55,0.8,0.55,.1),texte=c("$F_{\\infty}$","$Y_k$"),col=rep("black",2)),
    list(posy=c(0.12,0.45),posz=c(0.8,0.55,0.8,0.55,.1),texte=c("$F_{\\infty}$","$Y_k$"),col=rep("black",2)),
    list(posy=c(0.12,0.45),posz=c(0.8,0.55,0.8,0.55,.1),texte=c("$F_{\\infty}$","$Y_k$"),col=rep("black",2)),
    list(posy=c(0.12,0.45),posz=c(0.8,0.55,0.8,0.55,.1),texte=c("$F_{\\infty}$","$Y_k$"),col=rep("black",2)),
    list(posy=c(0.12,0.45),posz=c(0.8,0.55,0.8,0.55,.1),texte=c("$F_{\\infty}$","$Y_k$"),col=rep("black",2)),
    list(posy=c(1),posz=c(.1),texte="",col="black"))
  taillegraph<-list(c(4,5),c(8,5),c(8,5),c(8,5),c(8,5),c(8,5),c(8,5),c(8,5))
  listcouleur<-list(sample="orangeensai",pop="black",lim="bleuensai1",soft="gray")
  listtaille<-list(fin=0.6,axes=0.6,epais=1.2,ptss=2,pts=.6)
  repy=list(NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL);
  repz<-repy
#procedure
plotgen<-function(fic3,Gg,b,ker=ker,m=m,prop1,prop2,taillegraph,demarc=NULL,listcouleur=listcouleur,listtaille=listtaille,repy,repz,maxy,maxz,maxz3,quoigen){
  tailles<-list(ptl=listtaille$fin,axes=listtaille$axes,courbe=rep(listtaille$fin,3),scdf=listtaille$epais,cdf=listtaille$epais,ptss=listtaille$ptss,pts=listtaille$pts)
  couleurs<-list(ptss=listcouleur$sample,pts=listcouleur$pop,cdf=listcouleur$pop,scdf=listcouleur$sample,lim=listcouleur$lim,ptl=listcouleur$soft,courbe=c(listcouleur$pop,listcouleur$sample,listcouleur$lim))
  
  x<-round(maxy*seq(prop1[1],prop2[3],length.out = 300),4);
  #a.
  if(max(quoigen==1)|max(quoigen==2)|max(quoigen==3)|max(quoigen==4)){Y<-cbind(m$ploi(x),m$ploilim(x),m$ploilim(x))}
  if(max(quoigen==1)){fic=paste(fic3,"a.tex",sep="");
  quoi<-c("deb","axes","spts","texte","grades","ppts","strate","ligneh","fin");
  generecodetexgraph(fic,Gg$Yg,Gg$Sg,Gg$Zg,taillegraph[[1]],prop2,quoi,x,Y,repy[[1]],repz[[1]],legende[[1]],couleurs,Gg$demarc,tailles,maxy,maxz)}
  #b.limit sample cdf and population cdf
   if(max(quoigen==2)){ quoi=c("deb","axes","courbe","texte","grades","fin");
  fic=paste(fic3,"b.tex",sep="");
  texte<-c("$F$","$F_\\infty$"); tailles$courbe<-rep(listtaille$epais,2)
  generecodetexgraph(fic,Gg$Yg,Gg$Sg,Gg$Zg,taillegraph[[2]],prop1,quoi,x,Y,repy[[2]],repz[[2]],legende[[2]],couleurs,Gg$demarc,tailles,maxy,1)}
  #c.empirical population cdf
  if(max(quoigen==3)){quoi<-c("deb","axes","courbe","fdr","grades","fin");
  fic<-paste(fic3,"c.tex",sep=""); tailles$courbe<-rep(listtaille$fin,2)
  generecodetexgraph(fic,Gg$Yg,Gg$Sg,Gg$Zg,taillegraph[[3]],prop1,quoi,x,Y,repy[[3]],repz[[3]],legende[[2]],couleurs,Gg$demarc,tailles,maxy,1)}
  #d.empirical sample cdf
  if(max(quoigen==4)){quoi=c("deb","axes","courbe","texte","fdremp","grades","fin")
  fic=paste(fic3,"d.tex",sep="");
  texte<-c("$\\alpha$");texte<-c("") 
  generecodetexgraph(fic,Gg$Yg,Gg$Sg,Gg$Zg,taillegraph[[4]],prop1,quoi,x,Y,repy[[4]],repz[[4]],legende[[2]],couleurs,Gg$demarc,tailles,maxy,1)}
  
  if(max(quoigen==5)){kdey<-function(x){round(as.vector(kde(Gg$Yg[Gg$Sg],hpi(x=Gg$Yg[Gg$Sg])/2,eval.points=x)$estimate),4)}
  Y<-cbind(m$dloi(x),kdey(x),m$dloi(x)*m$rho(x));
  quoi=c("deb","axes","courbe","texte","fin")
  fic=paste(fic3,"e.tex",sep="");
  echelled<-max(Y)
  texte<-c("$\\alpha$");texte<-c("") 
  generecodetexgraph(fic,Gg$Yg,Gg$Sg,Gg$Zg,taillegraph[[5]],prop1,quoi,x,Y,repy[[5]],repz[[5]],legende[[2]],couleurs,Gg$demarc,tailles,maxy,1)
}

  y0<-x
  #Gg<-list(Yg=y,Sg=s,N=length(y),n=length(y[s]));ker=kergaus;m
  if(max(quoigen==6)){Y<-cbind(m$dloi(x),p2sr(y0,Gg,b,ker=ker,m=m),fHT(y0,Gg,b,ker=ker,m=m));
  quoi=c("deb","axes","courbe","texte","fin")
  fic=paste(fic3,"f.tex",sep="");
  texte<-c("$\\alpha$");
  generecodetexgraph(fic,Gg$Yg,Gg$Sg,Gg$Zg,taillegraph[[6]],prop1,quoi,x,Y,repy[[6]],repz[[6]],legende[[2]],couleurs,Gg$demarc,tailles,maxy,1)}
  if(max(quoigen==7)){fic=paste(fic3,"g.tex",sep="");
  Y<-cbind(m$dloi(x),p2sr(y0,Gg,b,ker=ker,m=m),p2(y0,Gg,b,ker=ker,m=m));
  generecodetexgraph(fic,Gg$Yg,Gg$Sg,Gg$Zg,taillegraph[[7]],prop1,quoi,x,Y,repy[[7]],repz[[7]],legende[[2]],couleurs,Gg$demarc,tailles,maxy,1)}
  if(max(quoigen==8)){
  couleurs$courbe<-c(listcouleur$lim,listcouleur$sample)
    quoi=c("deb","axes","courbe","texte","fin")
    y0<-seq(m$support[1],m$support[2],length.out=100);
    mmts<-moments(y0,b,ker=ker,m=m,lafun=p,N=Gg$N,nrep=1000);
    vpemp<-mmts$var;
    vp<-varp(y0,b=b,ker=ker,m=m,N=N);
    maxx=max(c(vpemp,vp))
    fic<-paste(fic3,"h.tex",sep="");
    leg<-list(posy=c(0),posz=c(1),texte=c(paste(maxx,"",sep="")),col=c("black"))
    Y<-cbind(vpemp,vp)/maxx
    repz=c(1)
    generecodetexgraph(fic,Gg$Yg,Gg$Sg,Gg$Zg,taillegraph[[8]],prop1,quoi,y0,Y,repy,repz,leg,couleurs,Gg$demarc,tailles,maxy,1)
  }}

