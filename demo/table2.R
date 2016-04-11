########################################################
##5.2.3. stratification et covariables
########################################################
########################################################
#library(pubBonneryBreidtCoquet2016)
popmodelfunction<-model.dep.strat2;
sampleparam<-list(proph=c(.7,.3),tauh=c(1/70,2/15))
N<-5000;
theta=c(.5,1,2);xi=2;EX=1;SX=1
Param<-list(list(sigma=.1,EX=EX,SX=SX),
            list(sigma=1 ,EX=EX,SX=SX),
            list(sigma=10,EX=EX,SX=SX));
#Simulation parameters
nbreps<-1000;
nomparam=paste("$\\begin{bmatrix}\\","sigma","\\\\\\mu_X\\\\\\","sigma_X","\\","end{bmatrix}$",sep='')
#Simulation
set.seed(1);
simulation_data<-lapply(Param,function(param){Simulation_data(nbreps,popmodelfunction,sampleparam,N,theta,xi,param,method)})
x=generetableau(simulation_data,nomparam)



