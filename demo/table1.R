set.seed(1)#NB: the seed was not set for the table in the publication
popmodelfunction<-model.Pareto.bernstrat;
sampleparam<-list(tauh=c(0.01,0.1));N<-10000;theta<-4;Xi=list(.1,1,2);
param<-list();
table1_data<-lapply(Xi,function(xi){Simulation_data(popmodelfunction,sampleparam,N,theta,xi,param,nbreps=3)})
table1<-simulation.summary(table1_data)
