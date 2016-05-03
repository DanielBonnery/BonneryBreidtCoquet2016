set.seed(1)#NB: the seed was not set for the table in the publication
popmodelfunction<-model.Pareto.bernstrat;
sampleparam<-list(tauh=c(0.01,0.1));N<-10000;theta<-4;Xi=list(.1,1,2);
param<-list();nbreps<-3;
table1_data<-lapply(Xi,function(xi){Simulation_data(nbreps,popmodelfunction,sampleparam,N,theta,xi,param)})
table1<-lapply(table1_data,simulation.summary())