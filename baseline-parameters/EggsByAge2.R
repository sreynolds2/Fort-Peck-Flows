
library(truncnorm)
library(lme4)
library(merTools)

# UPPER RIVER VON BERTALANFFY GROWTH PARAMETER DISTRIBUTION
## PULL JAGS FIT W/ Linf TRUNCATED AT 1800 
fn<- dir("../PDSG-growth/_outputs/_model-fits")
fn<- fn[grep("vbgf", fn)]
fn<- fn[grep("unknown-age_1800.RDS", fn)]
out<- readRDS(paste0("../PDSG-growth/_outputs/_model-fits/", fn))
fit<- out$fit
Linf<- fit$BUGSoutput$mean$Linf
k<- fit$BUGSoutput$mean$k
t0<- fit$BUGSoutput$mean$t0
sigma<- fit$BUGSoutput$mean$sigma
delta<- fit$BUGSoutput$mean$delta

# AGE-LENGTH RELATIONSHIP
length_at_age<- function(age=NULL,
                         reps=NULL,
                         Linf=NULL,
                         k=NULL,
                         t0=NULL,
                         sigma=NULL,
                         delta=NULL)
{
  La<- Linf*(1-exp(-k*(age-t0)))
  tau<- sigma*age^delta
  l<-rnorm(reps, La, tau)
  for(i in 1:length(l))
  {
    l[i]<- ifelse(l[i]<0, La, l[i])
    l[i]<- ifelse(l[i]>1800+2*sigma*100^delta, 
                  1800+2*sigma*100^delta, l[i])
    
  }
  return(l)
}

# UPPER RIVER LENGTH-FECUNDITY RELATIONSHIP
dat<-read.csv("../fecundity/_dat/Fecundity.csv")
## FILL MISSING VALUES AS NA
dat[dat==-99]<-NA
## ADD AN INDVIDUAL ID
dat$id<-1:nrow(dat)
## MEAN AND SD TO SCALE VARIABLES
mean_fl<-mean(na.omit(dat$FL))
sd_fl<-sd(na.omit(dat$FL))
## SCALE DATA 
dat$fl_std<-scale(dat$FL, center = mean_fl, scale = sd_fl)
## FIT MODEL GIVEN DATA (FIT 7 USED IN POP MODEL)
## VERIFY ALL ASSUMPTIONS MET
fit<-glmer(EGGS~fl_std+(1|id),dat[which(dat$BASIN=="Upper"),],
           family=poisson(link="log"))
intrcpt<- unname(fixef(fit)[1])
slp<- unname(fixef(fit)[2])
disp<- (sd(unlist(ranef(fit)$id))*(length(unlist(ranef(fit)$id))-1)/length(unlist(ranef(fit)$id))
        +sd(unlist(ranef(fit)$id)))/2
# plot(dat$FL, dat$EGGS)
# points(800:1700, exp(intrcpt+slp*scale(800:1700, center = mean_fl, scale = sd_fl)), 
#        col="blue", type="l")

eggs<- function(fork_length=NULL,
                a=NULL,
                b=NULL,
                dispersion_param=NULL)
{
  N<-length(fork_length)
  fl_normalized<- (fork_length - mean_fl)/sd_fl
  egg_no<- rpois(N,exp(rnorm(N,a + b*fl_normalized,dispersion_param)))
  return(egg_no)
}


## SIMULATE FECUNDITY BY AGE
n=1000000
a=1:100
fec<- lapply(a, function(x)
{
  lengths<- length_at_age(age=x, reps=n, Linf=Linf,
                          k=k, t0=t0, sigma=sigma, delta=delta)
  mn_lgth<- mean(lengths)
  med_lgth<- median(lengths)
  lgth_1400_plus<- length(which(lengths>=1400))/length(lengths)
  fecundity<- eggs(fork_length=lengths, a=intrcpt, b=slp, dispersion_param=disp)
  mn_eggs<- mean(fecundity)
  med_eggs<- median(fecundity) 
  return(list(mean_eggs=mn_eggs, median_eggs=med_eggs, 
              max_eggs=max(fecundity), min_eggs=min(fecundity),
              mean_length=mn_lgth, median_length=med_lgth,
              proportion_1400_plus=lgth_1400_plus))
})
fecundity<- data.frame(Age=a, 
                       Mean_Eggs_Produced=sapply(fec, "[[", 1),
                       Median_Eggs_Produced=sapply(fec, "[[", 2),
                       Max_Eggs_Simulated=sapply(fec, "[[", 3),
                       Min_Eggs_Simulated=sapply(fec, "[[", 4),
                       Mean_Length=sapply(fec, "[[", 5),
                       Median_Length=sapply(fec, "[[", 6),
                       Proportion_Length_1400plus=sapply(fec, "[[", 7))
write.csv(fecundity, "./baseline-parameters/fecundity_estimates_by_age_100_new.csv",
          row.names = FALSE)

# fecundity<- data.frame(Age=101, 
#                        Mean_Eggs_Produced=mn_eggs,
#                        Median_Eggs_Produced=med_eggs,
#                        Max_Eggs_Simulated=max(fecundity),
#                        Min_Eggs_Simulated=min(fecundity),
#                        Mean_Length=mn_lgth,
#                        Median_Length=med_lgth,
#                        Proportion_Length_1400plus=lgth_1400_plus)
# write.csv(fecundity, "./baseline-parameters/fecundity_estimates_age_101.csv",
#           row.names = FALSE)

# MEAN FECUNDITY INCREASES LINEARLY WITH AGE AND IS CLOSE TO 100,000 BY AGE 60 
# THIS MAY BE EXPECTED BASED ON THE MATH--CHECK!
# DO WE EXPECT THIS IN REALITY?
# MEDIAN FECUNDITY INCREASES WITH SLIGHT DIMINISHING RETURNS AND IS MUCH LOWER

fecundity<- read.csv("./baseline-parameters/fecundity_estimates_by_age_100_new.csv")

plot(fecundity$Age, fecundity$Mean_Eggs_Produced/1000, xlab="Age (Years)", 
     ylab="Thousands of Eggs Produced Per Female", pch=19)
points(fecundity$Age, fecundity$Median_Eggs_Produced/1000, col="blue")

# MEAN COMPARISONS
age<- 1:100
fl<- Linf*(1-exp(-k*(age-t0)))
fl_normalized<- (fl - mean_fl)/sd_fl
eggs<- exp(intrcpt + slp*fl_normalized)
points(age, eggs/1000, col="green")
# MEAN OF MEAN IS APPROXIMATELY MEDIAN OF DISTRIBUTION
