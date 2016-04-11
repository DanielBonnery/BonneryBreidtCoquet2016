#library(pubBonneryBreidtCoquet2016)
#############################################################
##5.5.2. stratification.
popmodelfunction<-model.dep.strat;
sampleparam<-list(proph=c(.7,.3),tauh=c(1/70,2/15));
Param<-list(list(sigma=0.1),list(sigma=1),list(sigma=10));
N<-5000;nbreps<-400;
theta=1.5;xi=2

nomparam="$\\sigma$"
method="grille"
simulation_data<-lapply(Param,function(param){Simulation_data(nbreps,popmodelfunction,sampleparam,N,theta,xi,param,method)})
generetableau(simulation_data,nomparam)