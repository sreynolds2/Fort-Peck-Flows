library(RODBC)

chan<-odbcConnectExcel2007("./dat/UMORdrift_forPopModel_v1.xlsx")
saklevels<-sqlFetch(chan,"SakLevels")
scen<-sqlFetch(chan,"0pt5mps")
dat<-scen
scen<-sqlFetch(chan,"0pt7mps")
dat<-rbind(dat,scen)
scen<-sqlFetch(chan,"0pt9mps")
dat<-rbind(dat,scen)
odbcClose(chan)
rm(chan,scen)


## SUMMARIZE THE PROPORTION OF AGE-0 RETAINED IN SYSTEM
saklevels$id<-1:nrow(saklevels)


combos<- expand.grid(U_mps=unique(dat$U_mps),
    temp_C=unique(dat$temp_C),
    id=saklevels$id,
    p0=seq(0.000001,0.001,length.out=50))
combos<-merge(combos,saklevels,by="id")
combos$lambda<-0

check<-rep(0,nrow(combos))
for(i in 1:nrow(combos))
    {
    dd<-subset(dat,(U_mps==combos$U_mps[i]&
        temp_C==combos$temp_C[i]))
    pp<-approxfun(x=dd$RM,
        y=dd$Percentile,rule=2)
    retained<-pp(combos$RM[i])  
      # PERCENTILE MUST BE THE PROPORTION OF FISH UP RIVER FROM THE
      # GIVEN RIVER MILE, CORRECT?
    maxAge<-41
    sexratio<-0.5
    ageMat<-9
    fec<- 19064
    p<- 0.922
    spnInt<- 1/3
    p0<- combos$p0[i] # 0.00001
    p1<-0.68
    S<-c(p1,rep(p,maxAge-2))
    A<-matrix(0,maxAge,maxAge)
    A[1,ageMat:maxAge]<- sexratio*fec*spnInt*p0*retained
    A[cbind(2:maxAge,1:(maxAge-1))]<-S
    check[i]<- which.max(abs(Re(eigen(A)$values)))
    combos$lambda[i]<-as.numeric(eigen(A)$values[1])
    }

lattice::levelplot(lambda~RM+p0,combos,
    subset=c(U_mps==0.5, temp_C==16)) 
library(plyr)
library(reshape2)
pp<- expand.grid(U_mps=c(0.5,0.7,0.9),
    temp_C=c(14,16,18))
    
#par(mfrow=c(3,3),mar=c(2,1,1,0),
#    oma=c(2,2,1,1))

pdf(file="lambdas.pdf")
for(i in 1:nrow(pp))
    {
    xx<-dcast(combos,RM~p0,value.var="lambda",
        mean,subset=.(U_mps==pp$U_mps[i]&
        temp_C==pp$temp_C[i]))
    x<-xx[,1]
    y<-sort(unique(combos$p0))
    z<-as.matrix(xx[,-1]) 
    tit<-paste("Velocity= ",
        combos$U_mps[i],"mps", 
        " & Temperature = ", 
        combos$temp_C[i],"C",sep="")
    contour(x=x,y=y,z=z,
        xlab="Lake Sakakawea Level (River mile)",
        ylab="Age-0 survival",
        main=tit,cex.main=1.5,cex.lab=1.3)
    }
dev.off()  

pdf(file="lambdas2.pdf")
par(mfrow=c(3,3),
    oma = c(4,5,2,3) + 0.1,
    mar = c(0.75,0.75,0.75,0.75) + 0.1,
    las=1)
for(i in 1:nrow(pp))
{
  xx<-dcast(combos,RM~p0,value.var="lambda",
            mean,subset=.(U_mps==pp$U_mps[i]&
                            temp_C==pp$temp_C[i]))
  x<-xx[,1]
  y<-sort(unique(combos$p0))
  z<-as.matrix(xx[,-1]) 
  if(i %in% c(1,4))
  {
    contour(x=x,y=y,z=z,
            xlab="", xaxt="n", 
            ylab="", ylim=c(-5e-05, 1e-03),
            tck=0.03, mgp=c(2,0.5,0))
    axis(1, at = 1:6*10+1500, 
         labels = FALSE,
         tick = TRUE, tck=0.03, mgp=c(2,0.5,0))
    axis(3,at=axTicks(1),labels=FALSE, tck=0.03, mgp=c(2,0.5,0))
    axis(4,at=axTicks(2),labels=FALSE, tck=0.03, mgp=c(2,0.5,0))
    
    if(i==1){mtext("Velocity=0.5mps", 3, padj=-0.5, font=1)}
  }
  if(i %in% c(2,3,5,6))
  {
    contour(x=x,y=y,z=z,
            xlab="", xaxt="n",
            ylab="", yaxt="n", ylim=c(-5e-05, 1e-03))
    axis(1, at = 1:6*10+1500, 
         labels = FALSE,
         tick = TRUE, tck=0.03, mgp=c(2,0.5,0))
    axis(2, at = 0:5*2e-04, 
         labels = FALSE,
         tick = TRUE, tck=0.03, mgp=c(2,0.5,0))
    axis(3,at=axTicks(1),labels=FALSE, tck=0.03, mgp=c(2,0.5,0))
    axis(4,at=axTicks(2),labels=FALSE, tck=0.03, mgp=c(2,0.5,0))
    if(i==2){mtext("Velocity=0.7mps", 3, padj=-0.5, font=1)}
    if(i==3)
    {
      mtext("Velocity=0.9mps", 3, padj=-0.5, font=1)
      mtext(expression(paste("14",degree,"C")), 4, adj=-0.2)
    }
    if(i==6){mtext(expression(paste("16",degree,"C")), 4, adj=-0.2)}
  }
  if(i %in% c(8,9))
  {
    contour(x=x,y=y,z=z,
            xlab="",
            ylab="", yaxt="n", ylim=c(-5e-05, 1e-03),
            tck=0.02, mgp=c(2,0.5,0))
    axis(2, at = 0:5*2e-04, 
         labels = FALSE,
         tick = TRUE, tck=0.03, mgp=c(2,0.5,0))
    axis(3,at=axTicks(1),labels=FALSE, tck=0.03, mgp=c(2,0.5,0))
    axis(4,at=axTicks(2),labels=FALSE, tck=0.03, mgp=c(2,0.5,0))
    if(i==9){mtext(expression(paste("18",degree,"C")), 4, adj=-0.2)}
  }
  if(i==7)
  {
    contour(x=x,y=y,z=z,
            xlab="", 
            ylab="", ylim=c(-5e-05, 1e-03),
            tck=0.03, mgp=c(2,0.5,0))
    axis(3,at=axTicks(1),labels=FALSE, tck=0.03, mgp=c(2,0.5,0))
    axis(4,at=axTicks(2),labels=FALSE, tck=0.03, mgp=c(2,0.5,0))
  }
}
mtext("Lake Sakakawea Level (River mile)", 1, outer=TRUE, padj=1, font=1)
mtext("Age-0 Survival Given Retention", 2, las=0,
      outer=TRUE, padj=-3, font=1)
dev.off()
    
plot(lambda~RM,combos,
    subset=U_mps==0.5); abline(1,0) 
dd<-subset(dat,(U_mps==combos$U_mps[9]&
                  temp_C==combos$temp_C[9]))
plot(Percentile~RM,dd,
     xlim=c(1200,1761))
abline(v=1761)# milk
abline(v=c(1565))# milk
abline(v=c(1507))# milk    
pp<-approxfun(x=dd$RM,
              y=dd$Percentile,rule=2)

pp(1400)


N<-40000
for(i in 2:40)
    {
    N<-c(N,N[i-1]*combos$lambda[1])    
    }

plot(N)
