set.seed(1)#NB: the seed was not set for the table in the publication
table1_data<-lapply(c(.1,1,2),function(xi){Simulation_data(popmodelfunction = model.Pareto.bernstrat,
                                                    sampleparam=list(tauh=c(0.01,0.1)),
                                                    N=10000,
                                                    theta=4,
                                                    xi=xi,
                                                    conditionalto=NULL)})
table1<-simulation.summary(table1_data)
