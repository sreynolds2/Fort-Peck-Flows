
source("./R/0_default-parameters.r")
source("./R/2_functions.r")
dat<- read.csv("./output/long-term_lambda_Standard_by_Year_DSM_no_zeros_new.csv",
               stringsAsFactors = FALSE)
alts<- c("NoAct", "1a", "1", "1b", "2a", "2", "2b")
# R1
dat$freq<- 1
spn_no<- aggregate(freq~Flow_Scenario, dat, sum)
spn_no<- spn_no[match(alts, spn_no$Flow_Scenario),]
par(mfrow=c(1,1))
barplot(spn_no$freq/83, space=0, names.arg = spn_no$Flow_Scenario, 
        ylim=c(0,0.16), xlab="Flow Alternative", 
        ylab="Percentage of Years with Spawning")

# R2
par(mfrow=c(3,3))
tmp<- dat[which(dat$Flow_Scenario==alts[1]),]
hist(tmp$Retention, 10, freq=FALSE, ylim=c(0,5), xlab="", ylab="",
     main="No Action Alternative", col="darkgray")
plot.new()
plot.new()
invisible(lapply(alts[2:7], function(x)
{
  tmp<- dat[which(dat$Flow_Scenario==x),]
  hist(tmp$Retention, 10, freq=FALSE, ylim=c(0,5), xlab="", ylab="",
       main=paste0("Alternative ", x), col="darkgray")
}))
mtext("Retention Probability", 1, outer=TRUE, padj=1)
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
