
source("./R/0_default-parameters.r")
source("./R/2_functions.r")
dat<- read.csv("./output/long-term_lambda_Standard_by_Year_DSM_no_zeros_new.csv",
               stringsAsFactors = FALSE)
alts<- c("NoAct", "1a", "1", "1b", "2a", "2", "2b")
dmar<- par()$mar
# R1
dat$freq<- 1
spn_no<- aggregate(freq~Flow_Scenario, dat, sum)
spn_no<- spn_no[match(alts, spn_no$Flow_Scenario),]
par(mfrow=c(1,1))
barplot(spn_no$freq/83, space=0, names.arg = spn_no$Flow_Scenario, 
        ylim=c(0,0.16), xlab="Flow Alternative", 
        ylab="Percentage of Years with Spawning")

# R2
mn_ret<- aggregate(Retention~Flow_Scenario, dat, mean)
mn_ret<- mn_ret[match(alts, mn_ret$Flow_Scenario),]
par(mfrow=c(3,3),
    mar=c(2,4,2,2)+0.1,
    oma=c(2,1,0,0))
tmp<- dat[which(dat$Flow_Scenario==alts[1]),]
hist(tmp$Retention, 10, freq=FALSE, ylim=c(0,5), xlab="", ylab="",
     main="No Action Alternative", col="darkgray")
legend(-0.05, 5.25, paste0("Spawning: ", round(spn_no$freq[1]/83,2), "%"), bty="n")
legend(-0.05, 4.6, paste0("Mean Retention: ", round(mn_ret$Retention[1],4)), bty="n")
plot.new()
plot.new()
invisible(lapply(2:7, function(x)
{
  tmp<- dat[which(dat$Flow_Scenario==alts[x]),]
  hist(tmp$Retention, 10, freq=FALSE, ylim=c(0,5), xlab="", ylab="",
       main=paste0("Alternative ", alts[x]), col="darkgray")
  legend(-0.05, 5.25, paste0("Spawning: ", round(spn_no$freq[x]/83,2), "%"), bty="n")
  legend(-0.05, 4.6, paste0("Mean Retention: ", round(mn_ret$Retention[x],4)), bty="n")
}))
mtext("Retention Probability", 1, outer=TRUE, padj=0.5)
mtext("Probability Density", 2, outer=TRUE, padj=1)


# R3
mn_LTPGR<- aggregate(Long_Term_Growth_Rate~Flow_Scenario, dat, mean)
mn_LTPGR<- mn_LTPGR[match(alts, mn_LTPGR$Flow_Scenario),]
par(mfrow=c(3,3))
tmp<- dat[which(dat$Flow_Scenario==alts[1]),]
hist(tmp$Long_Term_Growth_Rate, 10, freq=FALSE, ylim=c(0,50), 
     xlim=c(0.87, 0.98),xlab="", ylab="",
     main="No Action Alternative", col="darkgray")
legend(0.87,50, paste0("Spawning: ", round(spn_no$freq[1]/83,2), "%"), bty="n")
legend(0.87,43, paste0("Mean LTPGR: ", round(mn_LTPGR$Long_Term_Growth_Rate[1],4)), bty="n")
plot.new()
plot.new()
invisible(lapply(2:7, function(x)
{
  tmp<- dat[which(dat$Flow_Scenario==alts[x]),]
  hist(tmp$Long_Term_Growth_Rate, 10, freq=FALSE, ylim=c(0,50), xlab="", ylab="",
       main=paste0("Alternative ", alts[x]), col="darkgray")
  legend(0.87,50, paste0("Spawning: ", round(spn_no$freq[x]/83,2), "%"), bty="n")
  legend(0.87,43, paste0("Mean LTPGR: ", round(mn_LTPGR$Long_Term_Growth_Rate[x],4)), bty="n")
}))
mtext("Long-Term Population Growth Rate", 1, outer=TRUE, padj=1)
mtext("Probability Density", 2, outer=TRUE, padj=1)

max(mn_LTPGR$Long_Term_Growth_Rate)-min(mn_LTPGR$Long_Term_Growth_Rate)


# R4
## PLOT OF OUTCOMES WITH CHANGES IN AGE-0 SURVIVAL
### PULL 1985 STANDARD DATA
tmp<- dat[which(dat$Year==1985),]
LTLs<- lapply(1:nrow(tmp), function(x)
{
  inps<- inputs
  inps$p_retained<- tmp$Retention[x]
  inps$gamma<- 0.5
  out<- lapply(seq(0.00002, 0.002, 0.00002), function(y)
  {
    inps$phi0_MR<- y
    ea<- matrix_eigen_analysis(inps)
    outt<- data.frame(Flow_Scenario=tmp$Flow_Scenario[x],
                      Spawn_Date=tmp$Standard[x],
                      Retention=inps$p_retained,
                      phi0_MR=inps$phi0_MR,
                      Long_Term_Growth_Rate=ea$lambda1)
    return(outt)
  })
  out<- do.call(rbind, out)
})
LTLs<- do.call(rbind, LTLs)

par(mfrow=c(1,1))
alts2<- unique(tmp$Flow_Scenario)
cls<- c("black", "black", "gray", "gray")
typ<- c(1,6,1,6)
plot(LTLs[which(LTLs$Flow_Scenario==alts2[1]),]$phi0_MR,
     LTLs[which(LTLs$Flow_Scenario==alts2[1]),]$Long_Term_Growth_Rate,
     ylim=c(0.75, 1.1), xlab="", 
     ylab="Long-Term Population Growth Rate", type="l", lwd=2, lty=typ[1], 
     col=cls[1], tck=0.02, mgp=c(1.5,0.1,0))
invisible(lapply(2:length(alts2), function(i)
{
  points(LTLs[which(LTLs$Flow_Scenario==alts2[i]),]$phi0_MR,
         LTLs[which(LTLs$Flow_Scenario==alts2[i]),]$Long_Term_Growth_Rate,
         ylim=c(0.75, 1), xlab="", 
         ylab="Long-Term Population Growth Rate", type="l", lwd=2,
         col=cls[i], lty=typ[i]) 
}))
legend("bottomright", paste0("Alternative ", alts2), lwd=2, col=cls, 
       lty=typ, bty="n")
mtext("Age-0 Survival Given Retention", 1, outer=TRUE)


# R5
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

crvsB<-spawning_survival_retention_curves2(bnd_inps, phi0MR=0.00011)#103 in a million 
points(crvsB$gamma, crvsB$p_retained, 
       type="l", lty=3, col="gray")
points(0.5, dat[dat$Year==1983 & dat$Flow_Scenario=="1b",]$Retention,
       pch=1, col="black")
# points(0.5, dat[dat$Year==1983 & dat$Flow_Scenario=="2",]$Retention,
#        pch=16, col="black")
# 
# 
inps$gamma<- 0.5
inps$p_retained<- dat[dat$Year==1983 & dat$Flow_Scenario=="1b",]$Retention
inps$phi0_MR<- 0.000207
ea<- matrix_eigen_analysis(inps)
ea$lambda1
# 
# inps$p_retained<- dat[dat$Year==1983 & dat$Flow_Scenario=="2",]$Retention
# inps$phi0_MR<- 0.001225
# ea<- matrix_eigen_analysis(inps)
# ea$lambda1
# 
# 
# points(0.5, dat[dat$Year==1983 & dat$Flow_Scenario=="2b",]$Retention, 
#        pch=1, col="black")
points(0.5, dat[dat$Year==1983 & dat$Flow_Scenario=="1",]$Retention,
       pch=16, col="black")
# 
# 
# inps$gamma<- 0.5
# inps$p_retained<- dat[dat$Year==1983 & dat$Flow_Scenario=="2b",]$Retention
# inps$phi0_MR<- 0.000221
# ea<- matrix_eigen_analysis(inps)
# ea$lambda1

inps$p_retained<- dat[dat$Year==1983 & dat$Flow_Scenario=="1",]$Retention
inps$phi0_MR<- 0.000842
ea<- matrix_eigen_analysis(inps)
ea$lambda1


#NEED TO PULL UDAT FROM PDSG-growth
plot(udat$known[,1], udat$known[,2], xlab="Age", xlim=c(0,100), 
     ylab="Fork Length (mm)", ylim=c(0, 2000))
x<- seq(0,100,0.25)
points(x, Linf*(1-exp(-k*(x-t0))), type="l", col="gray", lwd=2)
points(x, Linf*(1-exp(-k*(x-t0)))+2*sigma*x^delta, type="l", 
       col="gray", lwd=1, lty=2)
points(x, Linf*(1-exp(-k*(x-t0)))-2*sigma*x^delta, type="l", 
       col="gray", lwd=1, lty=2)
mtext("Age", 1, padj=3)

plot(udat$known[,1], udat$known[,2], xlab="Age", xlim=c(0,25), 
     ylab="Fork Length (mm)")
x<- 0:25
points(x, Linf*(1-exp(-k*(x-t0))), type="l", col="gray", lwd=2)
