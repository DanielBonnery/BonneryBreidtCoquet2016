##Table 1
#library(pubBonneryBreidtCoquet2016)
popmodelfunction<-model.Pareto.bernstrat;
sampleparam<-list(tauh=c(0.01,0.1));N<-10000;Theta<-list(4,4,4);Xi=list(.1,1,2);
Param<-list(list());nbreps<-400;method="grille";
nomparam<-c("$\\theta$")

simulation_data<-Simulation_data(nbreps,popmodelfunction,sampleparam,N,Theta,Xi,Param,method)
generetableau(simulation_data,nomparam)
