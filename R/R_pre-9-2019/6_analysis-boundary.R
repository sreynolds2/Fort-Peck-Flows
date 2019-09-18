
# FIND LAMBDA=1 BOUNDARY

## FUNCTION
source("./R/1_global.r")
lambda_boundary<- function(maxage=NULL,
                           sexratio=NULL,
                           phi=NULL,
                           birthR=NULL,
                           phi0=NULL
                          )
{
  # BUILD LESLIE MATRIX
  A<-matrix(0,maxage,maxage)
  ## SURVIVAL RATES
  A[cbind(2:maxage,1:(maxage-1))]<- phi
  ## FECUNDITIES
  if(length(birthR)==1){birthV<- c(rep(0,8), rep(birthR, maxage-8))}
  if(length(birthR)==maxage){birthV<- birthR}
  A[1,] <- sexratio*birthV*phi0
  # A[1,] <- sexratio*spnProb*eggs*phi0
  
  # EIGENANALYSIS 
  ea<- eigen.analysis(A)
  ea$A<- A
  ea$phi0<- phi0
  ea$birth_rate<- round(sum(birthV*ea$stable.age)/sum(ea$stable.age[9:maxage]))
  # ea$birth_rate<- sum(spnProb*eggs*ea$stable.age)
  
  # PARAMETER SENSITIVITIES
  sens<- ea$sensitivities 
  ea$sensitivities<- list()
  ea$sensitivities$entries<- sens
  ea$sensitivities$phi<- rep(0, maxage-1)
  for(i in 1:(maxage-1))
  {
    ea$sensitivities$phi[i]<- sens[i+1,i]
  }
  ea$sensitivities$birthR<- sum(sens[1,]*sexratio*phi0)
  ea$sensitivities$phi0<- sum(sens[1,]*sexratio*birthR)
  ea$sensitivities$sexratio<- sum(sens[1,]*birthR*phi0)
  rm(sens)
  return(ea)
}


## FIND INITIAL BIRTH RATE AND AGE-0 SURVIVAL PAIR

bR<- c(seq(50, 950, 50), seq(1000, 15000, 100), seq(16000, 50000, 1000))
phi0_new<-0.01
out<- NULL
for(j in 1:length(bR))
{
  bR_init<- bR[j]
  phi0_init<- phi0_new

  tmp<- lambda_boundary(maxage = 60,
                        sexratio = 0.32,
                        phi = c(0.6369613, 0.7767894, rep(0.95, 57)),
                        phi0 = phi0_init,
                        birthR = bR_init)

  i<-0
  while(abs(tmp$lambda1-1)>0.0000001 & i<20)
  {
    dl<- 1-tmp$lambda1
    dphi0_max<- ifelse(dl<0, -tmp$phi0, 1-tmp$phi0)
    dphi0<- dl/tmp$sensitivities$phi0
    dphi0<- ifelse(abs(dphi0)>abs(dphi0_max), dphi0_max/2, dphi0)
    phi0_new<- tmp$phi0+dphi0
    tmp<- lambda_boundary(maxage = 60,
                          sexratio = 0.32,
                          phi = c(0.6369613, 0.7767894, rep(0.95, 57)),
                          phi0 = phi0_new,
                          birthR = bR_init)
    i<- i+1
  }
  out<- rbind(out, data.frame(birth_rate=bR_init, phi0=tmp$phi0, 
                              lambda=tmp$lambda1, iter=i,
                              dbR=tmp$sensitivities$birthR, 
                              dphi0=tmp$sensitivities$phi0))
  if(j<length(bR))
  {
    dphi0_max<- -out$phi0[j]
    dphi0<- -out$dbR[j]*(bR[j+1]-bR[j])/out$phi0[j]
    dphi0<- ifelse(abs(dphi0)>abs(dphi0_max), dphi0_max/2, dphi0)
    phi0_new<- out$phi0[j]+dphi0
  }
}
saveRDS(out, "./output/lambda_boundary.rds")
plot(out$birth_rate, out$phi0, type="l",xlab="", 
     ylab="", tck=0.02, mgp=c(2,0.5,0))
mtext("Fertilized Eggs per Mature Female per Year", 1, outer=TRUE, font=2)
mtext("Age-0 Survival", 2, outer=TRUE, las=0, padj=1, font=2)

## TRANSLATE BOUNDARY INTO P_RETAINED VALUES (ASSUMING SURVIVAL IN LAKE SAK=0)
drift<- readRDS("./dat/drift_data.rds")
p_retained<- unique(drift$p_retained)
p_retained<- p_retained[order(p_retained)]
p_retained<- setdiff(p_retained, c(0, 1))

dat<- readRDS("./output/lambda_boundary.rds")

shift<- lapply(p_retained, function(x)
{
  out<-dat[, 1:2]
  out$p_retained<- x
  out$phi0_MR<- out$phi0/out$p_retained
  return(out)
})

shift<- do.call("rbind", shift)
shift<- shift[shift$phi0_MR<=1,]

check<- sapply(1:nrow(shift), function(i)
{
  tmp<- lambda_boundary(maxage = 60,
                        sexratio = 0.32,
                        phi = c(0.6369613, 0.7767894, rep(0.95, 57)),
                        phi0 = shift$p_retained[i]*shift$phi0_MR[i],
                        birthR = shift$birth_rate[i])
  return(tmp$lambda1)
})
all(abs(check-1)<0.0000001)

shift$lambda<- check
shift<- shift[ ,c("birth_rate", "phi0", "phi0_MR", "p_retained", "lambda")]


dat$phi0_MR<- dat$phi0
dat$p_retained<- 1
dat<- dat[ ,c("birth_rate", "phi0", "phi0_MR", "p_retained", "lambda")]
dat<- rbind(shift, dat)
saveRDS(dat, "./output/p_retained_boundaries.rds")

dat<- readRDS("./output/p_retained_boundaries.rds")

par(mfrow=c(1,1))
plot(dat[dat$p_retained==1,]$birth_rate, 
     dat[dat$p_retained==1,]$phi0_MR, 
     type="l", xlab="",  ylab="", tck=0.02, mgp=c(2,0.5,0))
mtext("Fertilized Eggs per Mature Female per Year", 1, outer=TRUE, font=2)
mtext("Missouri River Age-0 Survival", 2, outer=TRUE, las=0, padj=1, font=2)
lapply(unique(dat$p_retained), function(pr)
{
  tmp<- subset(dat, p_retained==pr)
  points(tmp$birth_rate, tmp$phi0_MR, type="l")
})


dat_cut<- subset(dat, phi0_MR<=0.0001 & birth_rate<=15000)
plot(dat_cut[dat_cut$p_retained==1,]$birth_rate, 
     dat_cut[dat_cut$p_retained==1,]$phi0_MR, 
     type="l", xlab="", #xlim=c(550,14600),
     ylab="", ylim=c(0,0.0001),#ylim=c(0.0000039, 0.0001),
     tck=0.02, mgp=c(2,0.5,0))
mtext("Fertilized Eggs per Mature Female per Year", 1, outer=TRUE, font=2)
mtext("Missouri River Age-0 Survival", 2, outer=TRUE, las=0, padj=1, font=2)
lapply(unique(dat_cut$p_retained), function(pr)
{
  tmp<- subset(dat_cut, p_retained==pr)
  points(tmp$birth_rate, tmp$phi0_MR, type="l")
})

points(10000, 0.00007, pch=19, col="red")
pr<-0.319
tmp<- subset(dat_cut, p_retained==pr)
points(tmp$birth_rate, tmp$phi0_MR, type="l", col="blue", lwd=3)


min_retained<- function(bR=NULL,
                        MR_phi0=NULL,
                        dat=NULL)
{
  tmp<- subset(dat, phi0_MR<=MR_phi0 & birth_rate<=bR)
  out<- min(tmp$p_retained)
  return(out)
}

min_retained(bR=10000,
             MR_phi0=0.00007,
             dat=dat)

