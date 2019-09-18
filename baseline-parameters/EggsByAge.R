
library(truncnorm)
library(lme4)
library(merTools)

# AGE-1 LENGTH DISTRIBUTION PARAMETERS (TRUNCATED NORMAL)
mu_l_1 <- 200 
sd_l_1 <- 35
min_l_1 <- 50
max_l_1 <- 350
age_1_length<- function(n=NULL,
                        mu=NULL,
                        sd=NULL,
                        min=NULL,
                        max=NULL)
{
  l<-rtruncnorm(n, a=min, b=max, mean=mu, sd=sd)
  return(l)
}

# UPPER RIVER VON BERTALANFFY GROWTH PARAMETER DISTRIBUTION
ln_Linf_mu<-7.136028770
ln_k_mu<- -3.003764445
vcv<- matrix(c(0.2768,-0.364,-0.364,0.6342), 
             nrow=2, ncol=2, byrow=TRUE)
growth_params<- function(n=NULL,
                         mu_ln_Linf=NULL,
                         mu_ln_k=NULL,
                         vcv=NULL)
{
  ln_B<-eigen(vcv)$vectors%*%matrix(c(sqrt(eigen(vcv)$values[1]),0,
                                      0,sqrt(eigen(vcv)$values[2])),
                                    2,2)
  z1<-rtruncnorm(n, -sqrt(-2*log(0.2)), sqrt(-2*log(0.2)), 
                 mean=0, sd=1)
  z2<-rtruncnorm(n, 
                 a=-sqrt(-2*log(0.2)-z1^2), 
                 b=sqrt(-2*log(0.2)-z1^2), 
                 mean=0, sd=1)
  Z<-matrix(c(z1, z2), 2, n, byrow = TRUE)
  X<-t(ln_B%*%Z+c(mu_ln_Linf,mu_ln_k))
  X<-exp(X)
  return(list(linf=X[,1],k=X[,2]))
}

# AGE-LENGTH RELATIONSHIP
length_at_age<- function(age=NULL,
                         length_at_age_1=NULL,
                         Linf=NULL,
                         k=NULL)
{
  l<-(Linf-length_at_age_1)*(1-exp(-k*(age-1)))+length_at_age_1
  return(l)
}

# UPPER RIVER LENGTH-FECUNDITY RELATIONSHIP
dat<-read.csv("./GitHub/fecundity/_dat/Fecundity.csv")
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
rm(fit)
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
a=8:60
fec<- lapply(a, function(x)
{
  lower_ls<-age_1_length(n=n, mu=mu_l_1, sd=sd_l_1, min=min_l_1, max=max_l_1)
  VBparams<- growth_params(n=n, mu_ln_Linf=ln_Linf_mu, mu_ln_k=ln_k_mu, 
                           vcv=vcv)
  # max(VBparams[[1]])  ##TOO LARGE OF FISH PRODUCED... NEW MODEL FIT?... 
  ##WHY WAS LOG USED FOR THE PARAMETERS???
  lengths<- length_at_age(age=x, length_at_age_1=lower_ls, Linf=VBparams[[1]],
                          k=VBparams[[2]])
  fecundity<- eggs(fork_length=lengths, a=intrcpt, b=slp, dispersion_param=disp)
  mn<- mean(fecundity)
  med<- median(fecundity) 
  return(list(mean_eggs=mn, median_eggs=med))
})
fecundity<- data.frame(Age=a, 
                       Mean_Eggs_Produced=sapply(fec, "[[", 1),
                       Median_Eggs_Produced=sapply(fec, "[[", 2))
#write.csv(fecundity, "./baseline-parameters/fecundity_estimates_by_age.csv",
#          row.names = FALSE)

# MEAN FECUNDITY INCREASES LINEARLY WITH AGE AND IS CLOSE TO 100,000 BY AGE 60 
# THIS MAY BE EXPECTED BASED ON THE MATH--CHECK!
# DO WE EXPECTE THIS IN REALITY?
# MEDIAN FECUNDITY INCREASES WITH SLIGHT DIMINISHING RETURNS AND IS MUCH LOWER

fecundity<- read.csv("./baseline-parameters/fecundity_estimates_by_age.csv")
plot(fecundity$Age, fecundity$Mean_Eggs_Produced/1000, xlab="Age (Years)", 
     ylab="Thousands of Eggs Produced Per Female", pch=19)
