#Code written by Jay breidt.
library(survey)
data(birth)
# Gestational age.
Y<-birth$GestAge
log_weight<-log(birth$weight)
n<-length(Y)
b_GA<-data.frame(birth,Y,log_weight)
birth.design<-svydesign(~1,weights=~weight,strata=~stratum,data=b_GA)
ht.fit<-svyglm(log_weight~Y,birth.design)
Xi<-round(ht.fit$coef[2],3)
##########################################################################
# Save SMLE's
Sigma2<-round(var(Y)*(n-1)/n,3)
Mu<-round(mean(Y)+Sigma2*Xi,3) #sample mle
#
##########################################################################
#Information matrix calculations.
Inf_11<-matrix(0,2,2)
Inf_11[1,1]<- 1/Sigma2 # wrt mu^2
Inf_11[1,2]<- -Xi/Sigma2
Inf_11[2,1]<- -Xi/Sigma2
Inf_11[2,2]<- 1/(2*Sigma2^2)+Xi^2/Sigma2 #wrt (sigma2)^2

Inf_12<-matrix(0,1,2)
Inf_12[1,1]<- -1   # wrt xi and mu
Inf_12[1,2]<- Xi # wrt xi and sigma2
##########################################################################
# Want to estimate the design covariance matrix Sigma of the score vector and xi_hat.
# To do this, construct suitable response variables, including a linearized version of xi_hat, add 
# them all to the data frame and design object, then use svytotal and vcov.
#
# Construct terms in score vector, dividing by weight.
score_mu<-(Y-Mu+Xi*Sigma2)/Sigma2
score_mu<-score_mu/(n*birth$weight)
score_sig2<- -1/(2*Sigma2)-(Xi/Sigma2)*(Y-Mu+Xi*Sigma2)+(Y-Mu+Xi*Sigma2)^2/(2*Sigma2^2)
score_sig2<-score_sig2/(n*birth$weight)
#
# Construct terms in linearized version of /hat/xi; that is,
# weighted mean of these terms is linearized approximation to /hat/xi.
# Use the sandwich form of the variance
#
#/hat V/left(/hat{/bm{B}}/right)=/left(Z_s^T W_s Z_s/right)^{-1}
#/left/{/underset{k,l/in s}{/sum/sum}/frac{/Delta_{kl}}{/pi_{kl}}/frac{/bm{z}_ke_k}{/pi_k}/frac{/bm{z}^T_le_l}{/pi_l}/right/}
#/left(Z_s^T W_s Z_s/right)^{-1}
#
# where $e_k=y_k-/bm{x}_k^T/hat{/bm{B}}$.
#  So we need the design matrix Z_s=[z_k^T]=[(1,y_k)], the weights W_s=diag(weight), and the residuals e_k.
# Caution: residuals(ht.fit) does not seem to work.
# 
e_k<-log(birth$weight)-ht.fit$coef[1]-ht.fit$coef[2]*Y
tau2<-sum(birth$weight*e_k^2)/sum(birth$weight)
Tau2<-round(tau2,3)

Z<-cbind(rep(1,n),Y)
W<-diag(birth$weight)
# Verify ht.fit$coef
tmp<-rbind(c(0,1))%*%solve(t(Z)%*%W%*%Z)%*%t(Z)
check_21<- c(tmp)
check_22<-c(tmp)*Y
xi_check<- c(tmp)*log(birth$weight)
xi_lin<-c(tmp)*e_k

tmp<-rbind(c(1,0))%*%solve(t(Z)%*%W%*%Z)%*%t(Z)
xi0_check<- c(tmp)*log(birth$weight)
check_11<- c(tmp)
check_12<-c(tmp)*Y
xi0_lin<-c(tmp)*e_k


# Add new variables to data frame and survey design object.
b_GA2<-data.frame(b_GA,xi_lin,xi_check,xi0_check,xi0_lin,check_11,check_12,check_21,check_22,score_mu,score_sig2)
birth.design<-svydesign(~1,weights=~weight,strata=~stratum,data=b_GA2)

# Check some computations to make sure we have the right xi_lin
# xi0_check and xi_check just verify the point estimates.
# xi0_lin and xi_lin verify the standard errors, compared to summary(ht.fit)
# check_ij verify (Z'WZ)^{-1}Z'WZ=Identity.
#
svytotal(~xi0_check+xi0_lin+xi_check+xi_lin+check_11+check_12+check_21+check_22,birth.design)
#
# Compute svytotal for all three variables then get covariance
# matrix estimate via vcov()
#
a<-svytotal(~score_mu+score_sig2+xi_lin,birth.design)
Sigma<-n*vcov(a) # design covariance matrix has implicit 1/n

V<-solve(Inf_11)%*%(Sigma[1:2,1:2]+t(Inf_12)%*%Sigma[3,3]%*%Inf_12-t(Inf_12)%*%Sigma[3,1:2]-Sigma[1:2,3]%*%Inf_12)%*%solve(Inf_11)

################################################################################
################################################################################
# Now simulate the unstratified design.
set.seed(2015)
N<-15000
n<-90
################################################################################
# Choose Xi0 so that E[(weight)^{-1}]=E[pi_k]\simeq sum(pi_k)/N = n/N.
Xi0<--log(n/N)+Tau2/2-Xi*Mu+Xi^2*Sigma2/2

nreps<-1000
a<-rep(0,nreps)
mu_SMLE<-a
mu_HT<-a
mu_OLS<-a
sig2<-a
xi<-a
nn<-a
var_mu_HT<-a
var_sig2_SMLE<-a
var_xi_HT<-a
var_mu_SMLE<-a
var_mu_OLS<-a
smallest_weight<-a

for(r in 1:nreps){
  y_k<-sort(rnorm(N,mean=Mu,sd=sqrt(Sigma2)))
  w_k<-exp(rnorm(N,mean=Xi0+Xi*y_k,sd=sqrt(Tau2)))
  tmp<-(w_k<1)
  smallest_weight[r]<-min(w_k)
  w_k[tmp]<-1
  pi_k<-1/w_k
  s<-sample(1:N,size=90,prob=pi_k,replace=FALSE)
  s<-sort(s)
  #I<-rbinom(N,size=1,prob=pi_k)
  nn[r]<-sum(pi_k)
  #s<-(1:N)[I==1]
  Y<-y_k[s]
  mu_OLS[r]<-mean(Y)
  weight<-w_k[s]
  #Strata<-Strata[s]
  log_weight<-log(w_k[s])
  n_s<-length(Y)
  var_mu_OLS[r]<-var(Y)/n_s
  b_GA<-data.frame(weight,Y,log_weight)
  birth.design<-svydesign(~1,weights=~weight,data=b_GA)
  ht.fit<-svyglm(log_weight~Y,birth.design)
  xi0<-ht.fit$coef[1]
  xi[r]<-ht.fit$coef[2]
  sig2[r]<-var(Y)*(n_s-1)/n_s
  mu_SMLE[r]<-mean(Y)+sig2[r]*xi[r] #sample mle
  out<-svymean(~Y,birth.design)
  mu_HT[r]<-coef(out)
  var_mu_HT[r]<-(SE(out))^2
  Inf_11<-matrix(0,2,2)
  Inf_11[1,1]<- 1/sig2[r] # wrt mu^2
  Inf_11[1,2]<- -xi[r]/sig2[r]
  Inf_11[2,1]<- -xi[r]/sig2[r]
  Inf_11[2,2]<- 1/(2*sig2[r]^2)+xi[r]^2/sig2[r]  #wrt (sigma2)^2
  Inf_12<-matrix(0,1,2)
  Inf_12[1,1]<- -1   # wrt xi and mu
  Inf_12[1,2]<- xi[r] # wrt xi and sigma2
  
  # Want to estimate the design covariance matrix Sigma of the score vector and xi_hat.
  # To do this, construct suitable response variables, including a linearized version of xi_hat, add 
  # them all to the data frame and design object, then use svytotal and vcov.
  #
  # Construct terms in score vector, dividing by weight.
  score_mu<-(Y-mu_SMLE[r]+xi[r]*sig2[r])/sig2[r]
  score_mu<-score_mu/(n_s*weight)
  score_sig2<- -1/(2*sig2[r])-(xi[r]/sig2[r])*(Y-mu_SMLE[r]+xi[r]*sig2[r])+(Y-mu_SMLE[r]+xi[r]*sig2[r])^2/(2*sig2[r]^2)
  score_sig2<-score_sig2/(n_s*weight)
  #
  # Construct terms in linearized version of /hat/xi; that is,
  # weighted mean of these terms is linearized approximation to /hat/xi.
  # 
  e_k<-log(weight)-ht.fit$coef[1]-ht.fit$coef[2]*Y
  Z<-cbind(rep(1,n_s),Y)
  W<-diag(weight)
  tmp<-rbind(c(0,1))%*%solve(t(Z)%*%W%*%Z)%*%t(Z)
  xi_lin<-c(tmp)*e_k
  # Add new variables to data frame and survey design object.
  b_GA2<-data.frame(weight,Y,log_weight,xi_lin,score_mu,score_sig2)
  birth.design<-svydesign(~1,weights=~weight,data=b_GA2)
  a<-svytotal(~score_mu+score_sig2+xi_lin,birth.design)
  Sigma<-n_s*vcov(a)
  V<-solve(Inf_11)%*%(Sigma[1:2,1:2]+t(Inf_12)%*%Sigma[3,3]%*%Inf_12-t(Inf_12)%*%Sigma[3,1:2]-Sigma[1:2,3]%*%Inf_12)%*%solve(Inf_11)
  var_mu_SMLE[r]<-V[1,1]/n_s
  var_sig2_SMLE[r]<-V[2,2]/n_s
  var_xi_HT[r]<-Sigma[3,3]/n_s
}# end of simulation loop
#######################################################################

#Naive, Pseudo, Sample
Naive<-c(mean(mu_OLS), 100*mean(mu_OLS-Mu)/Mu, mean((mu_OLS-Mu)^2)/mean((mu_SMLE-Mu)^2),var(mu_OLS),mean(var_mu_OLS),mean(var_mu_OLS)/var(mu_OLS))
Pseudo<-c(mean(mu_HT), 100*mean(mu_HT-Mu)/Mu, mean((mu_HT-Mu)^2)/mean((mu_SMLE-Mu)^2),var(mu_HT),mean(var_mu_HT),mean(var_mu_HT)/var(mu_HT))
Sample<-c(mean(mu_SMLE), 100*mean(mu_SMLE-Mu)/Mu, mean((mu_SMLE-Mu)^2)/mean((mu_SMLE-Mu)^2),var(mu_SMLE),mean(var_mu_SMLE),mean(var_mu_SMLE)/var(mu_SMLE))

out_un<-rbind(Naive,Pseudo,Sample)
round(out_un,3)

################################################################################
################################################################################
# Now simulate the stratified design.
set.seed(2015)
N<-15000
n<-90
################################################################################
# Choose Xi0 so that E[(weight)^{-1}]=E[pi_k]\simeq sum(pi_k)/N = n/N.
Xi0<--log(n/N)+Tau2/2-Xi*Mu+Xi^2*Sigma2/2

nreps<-1000
a<-rep(0,nreps)
mu_SMLE<-a
mu_HT<-a
mu_OLS<-a
sig2<-a
xi<-a
nn<-a
var_mu_HT<-a
var_sig2_SMLE<-a
var_xi_HT<-a
var_mu_SMLE<-a
var_mu_OLS<-a

smallest_weight<-a

for(r in 1:nreps){
  y_k<-sort(rnorm(N,mean=Mu,sd=sqrt(Sigma2)))
  w_k<-exp(rnorm(N,mean=Xi0+Xi*y_k,sd=sqrt(Tau2)))
  tmp<-(w_k<1)
  smallest_weight[r]<-min(w_k)
  w_k[tmp]<-1
  pi_k<-1/w_k
  Strata<-1+(floor(89.999*cumsum(pi_k)/(5*sum(pi_k))))
  s<-c()
  for(h in 1:18){
    s<-c(s,sample((1:N)[Strata==h],size=5,prob=pi_k[Strata==h],replace=FALSE))
  }
  s<-sort(s)
  #I<-rbinom(N,size=1,prob=pi_k)
  nn[r]<-sum(pi_k)
  #s<-(1:N)[I==1]
  Y<-y_k[s]
  mu_OLS[r]<-mean(Y)
  weight<-w_k[s]
  Strata<-Strata[s]
  log_weight<-log(w_k[s])
  n_s<-length(Y)
  var_mu_OLS[r]<-var(Y)/n_s
  b_GA<-data.frame(weight,Y,log_weight,Strata)
  birth.design<-svydesign(~1,weights=~weight,strata=~Strata,data=b_GA)
  ht.fit<-svyglm(log_weight~Y,birth.design)
  xi0<-ht.fit$coef[1]
  xi[r]<-ht.fit$coef[2]
  sig2[r]<-var(Y)*(n_s-1)/n_s
  mu_SMLE[r]<-mean(Y)+sig2[r]*xi[r] #sample mle
  out<-svymean(~Y,birth.design)
  mu_HT[r]<-coef(out)
  var_mu_HT[r]<-(SE(out))^2
  Inf_11<-matrix(0,2,2)
  Inf_11[1,1]<- 1/sig2[r] # wrt mu^2
  Inf_11[1,2]<- -xi[r]/sig2[r]
  Inf_11[2,1]<- -xi[r]/sig2[r]
  Inf_11[2,2]<- 1/(2*sig2[r]^2)+xi[r]^2/sig2[r]  #wrt (sigma2)^2
  Inf_12<-matrix(0,1,2)
  Inf_12[1,1]<- -1   # wrt xi and mu
  Inf_12[1,2]<- xi[r] # wrt xi and sigma2
  
  # Want to estimate the design covariance matrix Sigma of the score vector and xi_hat.
  # To do this, construct suitable response variables, including a linearized version of xi_hat, add 
  # them all to the data frame and design object, then use svytotal and vcov.
  #
  # Construct terms in score vector, dividing by weight.
  score_mu<-(Y-mu_SMLE[r]+xi[r]*sig2[r])/sig2[r]
  score_mu<-score_mu/(n_s*weight)
  score_sig2<- -1/(2*sig2[r])-(xi[r]/sig2[r])*(Y-mu_SMLE[r]+xi[r]*sig2[r])+(Y-mu_SMLE[r]+xi[r]*sig2[r])^2/(2*sig2[r]^2)
  score_sig2<-score_sig2/(n_s*weight)
  #
  # Construct terms in linearized version of /hat/xi; that is,
  # weighted mean of these terms is linearized approximation to /hat/xi.
  # 
  e_k<-log(weight)-ht.fit$coef[1]-ht.fit$coef[2]*Y
  Z<-cbind(rep(1,n_s),Y)
  W<-diag(weight)
  tmp<-rbind(c(0,1))%*%solve(t(Z)%*%W%*%Z)%*%t(Z)
  xi_lin<-c(tmp)*e_k
  # Add new variables to data frame and survey design object.
  b_GA2<-data.frame(weight,Y,log_weight,xi_lin,score_mu,score_sig2,Strata)
  birth.design<-svydesign(~1,weights=~weight,strata=~Strata,data=b_GA2)
  a<-svytotal(~score_mu+score_sig2+xi_lin,birth.design)
  Sigma<-n_s*vcov(a)
  V<-solve(Inf_11)%*%(Sigma[1:2,1:2]+t(Inf_12)%*%Sigma[3,3]%*%Inf_12-t(Inf_12)%*%Sigma[3,1:2]-Sigma[1:2,3]%*%Inf_12)%*%solve(Inf_11)
  var_mu_SMLE[r]<-V[1,1]/n_s
  var_sig2_SMLE[r]<-V[2,2]/n_s
  var_xi_HT[r]<-Sigma[3,3]/n_s
}# end of simulation loop
#######################################################################

#Naive, Pseudo, Sample
Naive<-c(mean(mu_OLS), 100*mean(mu_OLS-Mu)/Mu, mean((mu_OLS-Mu)^2)/mean((mu_SMLE-Mu)^2),var(mu_OLS),mean(var_mu_OLS),mean(var_mu_OLS)/var(mu_OLS))
Pseudo<-c(mean(mu_HT), 100*mean(mu_HT-Mu)/Mu, mean((mu_HT-Mu)^2)/mean((mu_SMLE-Mu)^2),var(mu_HT),mean(var_mu_HT),mean(var_mu_HT)/var(mu_HT))
Sample<-c(mean(mu_SMLE), 100*mean(mu_SMLE-Mu)/Mu, mean((mu_SMLE-Mu)^2)/mean((mu_SMLE-Mu)^2),var(mu_SMLE),mean(var_mu_SMLE),mean(var_mu_SMLE)/var(mu_SMLE))

out_st<-rbind(Naive,Pseudo,Sample)
table3<-rbind(data.frame(cbind(Selection="Unstratified",rownames(out_un),round(out_un,3))),
              data.frame(cbind(Selection="Stratified",rownames(out_st),round(out_st,3))))
colnames(table3)<-c("Selection","Estimator","Mean","% Relative Bias","RMSE Ratio","Empirical Variance","Average Estimated Variance","Variance Ratio")
rownames(table3)<-NULL

