
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








LS<- sort(unique(drift$Lake_Sak_Upper_RM))

par(mfrow=c(3,3),
    oma = c(1,2,0,0) + 0.1,
    mar = c(2,4,1,2) + 0.1,
    las=1)
lapply(LS, function(l)
{
  tmp<- subset(drift, Lake_Sak_Upper_RM==l)
  dat<- dcast(tmp, temp_C~U_mps, value.var="p_retained")
  x<-dat[,1]
  y<-sort(unique(tmp$U_mps))
  z<-as.matrix(dat[,-1])
  #cp<- contourLines(x=x,y=y,z=z, levels=0.615542)
  contour(x=x,y=y,z=z,
  xlab="",
  ylab="",
  main="",cex.main=1.5,cex.lab=1.3,
  tck=0.03, mgp=c(2,0.5,0))
  legend("topleft", LETTERS[which(LS==l)], bty="n")
})
mtext("Mean Temperature (C)", 1, outer=TRUE, font=2)
mtext("Mean Velocity (mps)", 2, las=0, outer=TRUE, font=2)

LS

par(mfrow=c(1,1))
l<-LS[5]
l
x<-rep(seq(14, 24, 2),3)
y<-rep(seq(0.5, 0.9, 0.2), each=6)
labs<-as.character(c(z[,1], z[,2], z[,3]))
plot(x,y, xlab="Mean Temperature (C)", ylab="Mean Velocity (mps)")


par(mfrow=c(2,3),
    oma = c(3,2,0,0) + 0.1,
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
  legend("topleft", LETTERS[which(temps==t)], bty="n")
})
mtext("Lake Sakakawea RM", 1, outer=TRUE, font=2)
mtext("Mean Velocity (mps)", 2, las=0, outer=TRUE, font=2)