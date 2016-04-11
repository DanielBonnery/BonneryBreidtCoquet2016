##Table 1
#library(pubBonneryBreidtCoquet2016)
popmodelfunction<-model.Pareto.bernstrat;
sampleparam<-list(tauh=c(0.01,0.1));N<-10000;theta<-4;Xi=list(.1,1,2);
param<-list();nbreps<-400;method="grille";
nomparam<-c("$\\theta$")
simulation_data<-lapply(Xi,function(xi){Simulation_data(nbreps,popmodelfunction,sampleparam,N,theta,xi,param,method)})
x=generetableau(simulation_data,nomparam)
try(system(paste0("evince ",x)))
