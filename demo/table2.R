#library(pubBonneryBreidtCoquet2016)
#############################################################
##5.5.2. stratification.
popmodelfunction<-model.dep.strat;
sampleparam<-list(proph=c(.7,.3),tauh=c(1/70,2/15));
Param<-list(list(sigma=0.1),list(sigma=1),list(sigma=10));
N<-5000;nbreps<-400;
Theta=list(c(1.5),c(1.5),c(1.5));Xi=list(2,2,2)
nomparam="$\\sigma$"
method="grille"
simulation_data<-Simulation_data(nbreps,popmodelfunction,sampleparam,N,Theta,Xi,Param,method)
generetableau(simulation_data,nomparam)