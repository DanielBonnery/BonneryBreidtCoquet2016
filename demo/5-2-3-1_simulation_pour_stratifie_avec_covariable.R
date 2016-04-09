########################################################
##5.2.3. stratification et covariables
########################################################

########################################################################################################
#Call all functions
library(pubBonneryBreidtCoquet2016)
# Definition of the population model 
popmodelfunction<-model.dep.strat2;
#sampling parameters
sampleparam<-list(proph=c(.7,.3),tauh=c(1/70,2/15))
#Population parameters
N<-5000;
theta=c(.5,1,2);xi=2;EX=1;SX=1
Theta=list(theta=theta,theta=theta,theta=theta);
Xi=list(xi=xi,xi=xi,xi=xi);
Param<-list(list(sigma=.1,EX=EX,SX=SX),
            list(sigma=1 ,EX=EX,SX=SX),
            list(sigma=10,EX=EX,SX=SX));
param=Param[[1]]
########################################################################################################
#Simulation (the one in the final document)
########################################################################################################
#Simulation parameters
nbreps<-1000;
fic="tableauStratcovar.tex";method="grilleIt";rep=paste("../",Version,sep="");
nomparam=paste("$\\begin{bmatrix}\\","sigma","\\\\\\mu_X\\\\\\","sigma_X","\\","end{bmatrix}$",sep='')
#Simulation
set.seed(1);

gege<-generetableau(nbreps,popmodelfunction,sampleparam,N,Theta,Xi,Param,fic,rep,method,nomparam)
save(gege,file="gege.RData")
system(paste("cd ../",Version, ";pdflatex testtableau.tex",sep=""))



if(FALSE){
m <- popmodelfunction(sampleparam,theta,xi,param)
tau=0.05
cav1000=m$cav(1000);cav1000$Im$I11;cav1000$Sigma[1:3,1:3]/(N*tau)
cav100=m$cav(100);cav100$Im$I11;cav100$Sigma[1:3,1:3]/(N*tau)
cav10000=m$cav(10000);cav10000$Im$I11;cav10000$Sigma[1:3,1:3]/(N*tau)


########################################################################################################
#Simulation (Test)
########################################################################################################
#Simulation parameters
                                        #Simulation



param=Param[[2]]

for(i in 1:10){
set.seed(i);
fic=paste("tableauStratcovar",i,".tex",sep='');
Param<-list(list(sigma=.1,EX=EX,SX=SX),
            list(sigma=1 ,EX=EX,SX=SX),
            list(sigma=10,EX=EX,SX=SX));
gege<-generetableau(nbreps,popmodelfunction,sampleparam,N,Theta,Xi,Param,fic,rep,method,nomparam)}
(round(popmodelfunction(sampleparam,theta,xi,param)[c("I11","I12","Sigma","V")]),4)

NN<-c(1000,2000,5000,5000,5000,5000,10000,20000, 50000,100000)
for(i in 1:10){
set.seed(i);
fic=paste("tableauStratcovar",i,".tex",sep='');
Param<-list(list(sigma=.1,EX=EX,SX=SX),
            list(sigma=1 ,EX=EX,SX=SX),
            list(sigma=10,EX=EX,SX=SX));
assign(paste("VAR",i,sep=''),popmodelfunction(sampleparam,theta,xi,param)
gege<-generetableau(nbreps,popmodelfunction,sampleparam,NN[i],Theta,Xi,Param,fic,rep,method,nomparam)}
  
}
