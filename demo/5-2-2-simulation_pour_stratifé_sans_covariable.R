#library(pubBonneryBreidtCoquet2016)
###############################################################
##5.5.2. stratification.
popmodelfunction<-model.dep.strat;
sampleparam<-list(proph=c(.7,.3),tauh=c(1/70,2/15));
Param<-list(list(sigma=0.1),list(sigma=1),list(sigma=10));
N<-5000;nbreps<-400;
fic="tableauStrat.tex";method="grille";rep="..";
Theta=list(c(1.5),c(1.5),c(1.5));Xi=list(2,2,2)
theta=1.5;xi=2;sigma=.1;param=list(sigma=sigma);
m<-popmodelfunction(sampleparam,theta,xi,param)
gege<-generetableau(nbreps,popmodelfunction,sampleparam,N,Theta,Xi,Param,fic,rep,method,"$\\sigma$")
#generetableau(nbreps=1000,popmodelfunction,sampleparam,N=20000,Theta=Theta,Xi=Xi,Param=Param,fic="testjay20000.tex",method="grille")
#generetableau(nbreps=1000,popmodelfunction,sampleparam,N=5000,Theta=Theta,Xi=Xi,Param=Param,fic="testjay5000.tex",method="grille")

##