
# EIS STANDARD DATA
dat<- read.csv("./output/long-term_lambda_Standard_by_Year_EIS_no_zeros.csv",
               stringsAsFactors = FALSE)
dat$Standard<- as.Date(dat$Standard)
dat$Hatch_Date<- as.Date(dat$Hatch_Date)

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
alts<- unique(tmp$Flow_Scenario)
cls<- c("darkgreen", "blue", "black", "red")
plot(LTLs[which(LTLs$Flow_Scenario==alts[1]),]$phi0_MR,
     LTLs[which(LTLs$Flow_Scenario==alts[1]),]$Long_Term_Growth_Rate,
     ylim=c(0.75, 1.1), xlab="", 
     ylab="Long-Term Population Growth Rate", type="l", lwd=2, 
     col=cls[1], tck=0.02, mgp=c(1.5,0.1,0))
invisible(lapply(2:length(alts), function(i)
{
  points(LTLs[which(LTLs$Flow_Scenario==alts[i]),]$phi0_MR,
         LTLs[which(LTLs$Flow_Scenario==alts[i]),]$Long_Term_Growth_Rate,
         ylim=c(0.75, 1), xlab="", 
         ylab="Long-Term Population Growth Rate", type="l", lwd=2,
         col=cls[i]) 
}))
legend("bottomright", paste0("Alternative ", alts), lwd=2, col=cls, 
       bty="n")
mtext("Age-0 Survival Given Retention", 1, outer=TRUE)


## HISTOGRAM OF LTPGRs GIVEN SPAWNING BY ALTERNATIVE 
par(mfrow=c(3,3))
tmp<- dat[which(dat$Flow_Scenario=="NoAct"),]
hist(tmp$Long_Term_Growth_Rate, 10, freq=FALSE, ylim=c(0,30), xlab="Long-Term Growth Rate", 
     main=paste0("Alternative ", "NoAct"), col="darkgray")
plot.new()
plot.new()
invisible(lapply(alts[1:6], function(x)
{
  tmp<- dat[which(dat$Flow_Scenario==x),]
  hist(tmp$Long_Term_Growth_Rate, 10, freq=FALSE, ylim=c(0,30), xlab="Long-Term Growth Rate", 
       main=paste0("Alternative ", x), col="darkgray")
}))


