setwd("./GitHub/Fort-Peck-Flows")

source("./R/0_default-parameters.r")
source("./R/1_global_DSM.r")
source("./R/2_functions.r")
fullrun_DSM<- FALSE
source("./R/3_load-and-clean_DSM.r")


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

inps<- inputs
bnd_inps<- boundary_product(inps)
crvs<- spawning_survival_retention_curves2(bnd_inps)
par(mfrow=c(1,1),
    oma=c(0,0,0,0),
    mar=c(3,3,1,1)+0.1)
plot_boundary_curves2(crvs)

crvsB<-spawning_survival_retention_curves2(bnd_inps, phi0MR=0.00011) 
points(crvsB$gamma, crvsB$p_retained, 
       type="l", lty=3, col="gray")


