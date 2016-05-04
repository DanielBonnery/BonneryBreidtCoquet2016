########################################################
## stratification et covariables
########################################################
set.seed(3)#NB: the seed was not set for the table in the publication
popmodelfunction<-model.dep.strat2;
sampleparam<-list(proph=c(.7,.3),tauh=c(1/70,2/15))
N<-5000;
theta=c(.5,1,2);xi=2;EX=1;SX=1
Conditionalto<-list(list(sigma=.1,EX=EX,SX=SX),
            list(sigma=1 ,EX=EX,SX=SX),
            list(sigma=10,EX=EX,SX=SX));
#Simulation
table2_data<-lapply(Conditionalto,function(conditionalto){Simulation_data(popmodelfunction,sampleparam,N,theta,xi,conditionalto)})
table2<-simulation.summary(table2_data)


