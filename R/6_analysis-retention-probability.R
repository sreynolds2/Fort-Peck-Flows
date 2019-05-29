
## P_RETAINED SCENARIOS
drift<- readRDS("./dat/drift_data.rds")
temps<- sort(unique(drift$temp_C))

par(mfrow=c(2,3),
    oma = c(1,2,0,0) + 0.1,
    mar = c(2,4,1,2) + 0.1,
    las=1)
lapply(temps, function(t)
{
  tmp<- subset(drift, temp_C==t)
  dat<- dcast(tmp, Lake_Sak_Upper_RM~U_mps, value.var="p_retained")
  x<-dat[,1]
  y<-sort(unique(tmp$U_mps))
  z<-as.matrix(dat[,-1])
  contour(x=x,y=y,z=z,
          xlab="",
          ylab="",
          main="",cex.main=1.5,cex.lab=1.3,
          tck=0.03, mgp=c(2,0.5,0))
  legend("topright", LETTERS[which(temps==t)], bty="n")
})
mtext("Lake Sakakawea RM", 1, outer=TRUE, font=2)
mtext("Mean Velocity (mps)", 2, las=0, outer=TRUE, font=2)



par(mfrow=c(2,3),
    oma = c(1,2,0,0) + 0.1,
    mar = c(2,4,1,2) + 0.1,
    las=1)
lapply(temps, function(t)
{
  tmp<- subset(drift, temp_C==t)
  dat<- dcast(tmp, Exc_value~U_mps, value.var="p_retained")
  x<-dat[,1]
  y<-sort(unique(tmp$U_mps))
  z<-as.matrix(dat[,-1])
  contour(x=x,y=y,z=z,
          xlab="",
          ylab="",
          main="",cex.main=1.5,cex.lab=1.3,
          tck=0.03, mgp=c(2,0.5,0))
  legend("topright", LETTERS[which(temps==t)], bty="n")
})
mtext("Lake Sakakawea RM", 1, outer=TRUE, font=2)
mtext("Mean Velocity (mps)", 2, las=0, outer=TRUE, font=2)