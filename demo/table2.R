########################################################
## stratification et covariables
########################################################
set.seed(1)#NB: the seed was not set for the table in the publication
table2_data<-lapply(lapply(c(.1,1,10),function(sigma){list(N=5000,sigma=sigma ,EX=1,SX=1,sampleparam=list(proph=c(.7,.3),tauh=c(1/70,2/15)))}),
                    function(conditionalto){message(paste0("Run simulations for sigma=",conditionalto$sigma));Simulation_data(popmodelfunction=model.dep.strat2,
                                                            theta=c(.5,1,2),
                                                            xi=2,
                                                            conditionalto=conditionalto)})
table2<-lapply(table2_data,simulation.summary)
table2

