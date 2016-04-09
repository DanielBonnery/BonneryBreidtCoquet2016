##5.2. Simulation procedure.
#library(pubBonneryBreidtCoquet2016)
###################################################################
##5.5.1. Pareto.
popmodelfunction<-model.Pareto.bernstrat;
sampleparam<-list(tauh=c(1/50,1/10));N<-10000;Theta<-list(4);Xi=list(2);
Param<-list(list());nbreps<-400;fic="tableauPareto.tex";method="grille";rep="..";
xi=2;theta=4
generetableau(nbreps,popmodelfunction,sampleparam,N,Theta,Xi,Param,fic,rep,method,"")
N=100;param=list();m<-popmodelfunction(sampleparam,theta,xi,param)
y<-m$rloiy(N);z<-m$rloiz(y);s<-m$Scheme$S(z);summary(z[s])
plot(y,z,col='gray20');points(y[s],z[s],col='red');
N=10000
y<-m$rloiy(N);z<-m$rloiz(y);s<-m$Scheme$S(z);summary(z[s])
yy<-sort(y[s])
zz<-as.vector(ks::kde(yy,0.005,eval.points=yy)$estimate)
zzz<-as.vector(ks::kde(yyy,0.0005,eval.points=yyy)$estimate)
yyy<-sort(y)
plot(yy,zz,type='l',col='black');
points(yy,m$dloi(yy)*m$rho(yy),type='l',col='blue');
points(yyy,m$dloi(yyy),type='l',col='red');
points(yyy,zzz,type='l',col='orange');
points(yy,m$rho(yy),type='l',col='green')
