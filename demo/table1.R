##5.2. Simulation procedure.
#library(pubBonneryBreidtCoquet2016)
###################################################################
##5.5.1. Pareto.
popmodelfunction<-model.Pareto.bernstrat;
sampleparam<-list(tauh=c(1/50,1/10));N<-10000;Theta<-list(4);Xi=list(2);
Param<-list(list());nbreps<-400;fic="tableauPareto.tex";method="grille";rep="..";
xi=2;theta=4
generetableau(nbreps,popmodelfunction,sampleparam,N,Theta,Xi,Param,fic,rep,method,"")
cat(readLines(fic))
