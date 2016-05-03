########################################################
##5.2.3. stratification et covariables
########################################################
########################################################
set.seed(1)#NB: the seed was not set for the table in the publication
popmodelfunction<-model.dep.strat2;
sampleparam<-list(proph=c(.7,.3),tauh=c(1/70,2/15))
N<-5000;
theta=c(.5,1,2);xi=2;EX=1;SX=1
Param<-list(list(sigma=.1,EX=EX,SX=SX),
            list(sigma=1 ,EX=EX,SX=SX),
            list(sigma=10,EX=EX,SX=SX));
#Simulation
table2_data<-lapply(Param,function(param){Simulation_data(popmodelfunction,sampleparam,N,theta,xi,param)})
table2<-lapply(table2_data,simulation.summary())


