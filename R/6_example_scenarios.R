setwd("./GitHub/Fort-Peck-Flows")

source("./R/1_global.r")
source("./R/2_functions.r")
full_run<- FALSE
source("./R/3_load-and-clean.r")

# Scenario 1
inps1<- inputs
inps1$gamma<- 0.5
inps1$p_retained<- 0.2 

l1<-matrix_eigen_analysis(inps1)
l1$lambda1

tbl1<-sens_elas_table(data=l1, number=56)
tbl1$Sensitivities[1:10,]
rbind(tbl1$Elasticities[1,],
      tbl1$Elasticities[which(tbl1$Elasticities$Elasticity==tbl1$Elasticities$Elasticity[2]),])
par(oma = c(2,10,1,0) + 0.1,
    mar = c(4,4,1,1) + 0.1)
barplot(tbl1$Sensitivities$Sensitivity[7:2], 
        names.arg=c("Sex Ratio", "Spawning Probability", "Maximum Age",
                    "Retention Probability", "Age-0 Survival Given Retention",
                    "Lake Sakakawea Age-0 Survival"), 
        xlab="Sensitivity Values", horiz=TRUE, las=1)

l1$sensitivities$entries[1,]
phiSens1<- rep(0, 59)
for(i in 1:59){phiSens1[[i]]<- l1$sensitivities$entries[i+1,i]}
phiSens1

# 
# library(popbio)
# maxage<- inputs$max_age  
# phi<- inputs$phi
# psi<- inputs$psi
# gamma<- inputs$gamma
# eggs<- inputs$eggs
# sexratio<- inputs$probF
# p_retained<- inputs$p_retained
# phi0_MR<- inputs$phi0_MR
# # BUILD LESLIE MATRIX
# A<- matrix(0,maxage,maxage)
# ## SURVIVAL VALUES
# A[cbind(2:maxage,1:(maxage-1))]<- phi
# ## FERTILITY VALUES
# A[1,] <- psi*gamma*eggs*sexratio*p_retained*phi0_MR
# Aminus<- A[1:(maxage-1), 1:(maxage-1)]
# Aplus<- rbind(A, c(rep(0, maxage-1), phi[maxage-1]))
# Aplus<- cbind(Aplus, c(100916.3, rep(0, maxage)))
# 
# sens<- sensitivity(A)
# sens[1,]
# 
# all(l1$sensitivities$entries[1,]==sens[1,])

# Scenario 2
inps2<- inputs
inps2$gamma<- 0.7
inps2$p_retained<- 0.05 

l2<-matrix_eigen_analysis(inps2)
l2$lambda1

tbl2<-sens_elas_table(data=l2, number=56)
tbl2$Sensitivities[1:10,]
rbind(tbl2$Elasticities[1,],
      tbl2$Elasticities[which(tbl2$Elasticities$Elasticity==tbl2$Elasticities$Elasticity[2]),])


par(oma = c(2,10,1,0) + 0.1,
    mar = c(4,4,1,1) + 0.1)
barplot(tbl2$Sensitivities$Sensitivity[7:2], 
        names.arg=c("Age-1 Survival", "Sex Ratio", "Maximum Age",
                    "Retention Probability", 
                    "Age-0 Survival Given Retention",
                    "Lake Sakakawea Age-0 Survival"), 
        xlab="Sensitivity Values", horiz=TRUE, las=1)


l2$sensitivities$entries[1,]
phiSens2<- rep(0, 59)
for(i in 1:59){phiSens2[[i]]<- l2$sensitivities$entries[i+1,i]}
phiSens2




spawning_survival_retention_curves2<- function(boundary_inputs=NULL,
                                               phi0MR=seq(0.0002, 0.002, 0.0002))
{
  maxage<- boundary_inputs$max_age
  phi<- boundary_inputs$phi
  psi<- boundary_inputs$psi
  eggs<- boundary_inputs$eggs
  sexratio<- boundary_inputs$probF
  prod<-boundary_inputs$boundary$product
  phi0<-phi0MR
  curve_dat<- lapply(1:length(phi0MR), function(i)
  {
    gamma<- c(0.000001,seq(0.01, 1, 0.01))
    p_retained<- prod/(phi0[i]*gamma)
    indx<-which(p_retained>1)
    p_retained<- p_retained[-indx]
    gamma<-gamma[-indx]
    #SYMMETRICAL SO CAN EXPAND
    tmp<-p_retained
    p_retained<- c(p_retained, gamma)
    gamma<- c(gamma, tmp)
    out<-NULL
    if(length(p_retained>0))
    {
      out<-data.frame(gamma=gamma, phi0_MR=phi0[i], p_retained=p_retained)
    }
    return(out)
  })
  curve_dat<- do.call(rbind, curve_dat)
  return(curve_dat)
}


plot_boundary_curves2<- function(curve_dat=NULL,
                                 gamma_upper=1,
                                 pret_upper=1,
                                 xlabel=expression(paste("Spawning Probability  (", gamma, ")")),
                                 ylabel=expression(paste("Retention Probability  (  ", p[ret], ")")),
                                 xaxis="s")
{
  phi0_MR<-unique(curve_dat$phi0_MR)
  phi0_MR<- phi0_MR[order(phi0_MR)]
  tmp<-curve_dat[which(curve_dat$phi0_MR==phi0_MR[1]),]
  tmp<-tmp[order(tmp$gamma),]
  plot(tmp$gamma, tmp$p_retained, type="l", 
       xlim=c(0,gamma_upper), ylim=c(0,pret_upper),
       xlab=xlabel, ylab=ylabel, xaxt=xaxis,
       tck=0.02, mgp=c(1.5,0.1,0))
  if(length(phi0_MR>1))
  {
    invisible(lapply(2:length(phi0_MR), function(i)
    {
      tmp<-curve_dat[which(curve_dat$phi0_MR==phi0_MR[i]),]
      tmp<-tmp[order(tmp$gamma),]
      points(tmp$gamma, tmp$p_retained, type="l")
    })) 
  }
}


bnd_inps1<- boundary_product(inps1)
bnd_inps2<- boundary_product(inps2)
bnd_inps1$boundary$product==bnd_inps2$boundary$product
bnd_inps1$boundary$check==bnd_inps2$boundary$check
bnd_inps1$boundary$check

crvs<- spawning_survival_retention_curves2(bnd_inps1)
plot_boundary_curves2(crvs)
points(0.5, 0.2, pch=19, col="blue")
points(0.7, 0.05, pch=19, col="red")
bnd_inps1$boundary$product/(0.7*0.05)
crvsB<-spawning_survival_retention_curves2(bnd_inps1, phi0MR=0.0034) 
points(crvsB$gamma, crvsB$p_retained, 
       type="l", lty=3, col="gray")

inps3<- inps1
inps3$phi0_MR<- 0.0012
l3<- matrix_eigen_analysis(inps3)
l3$lambda1

inps4<- inps2
inps4$phi0_MR<- 0.0034
l4<- matrix_eigen_analysis(inps4)
l4$lambda1
