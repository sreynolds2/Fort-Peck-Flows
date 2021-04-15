
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
# ## USED FOR THE COMPARISON DATA:
# ## mean(dat[which(dat$BASIN=="Upper"),]$FL, na.rm=TRUE) # 1475.72
# ## log(1475.72)    # 7.296901
# ## ln_Linf_mu<- 7.296901
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
dat<-read.csv("C:/Users/sreynolds/Documents/GitHub/fecundity/_dat/Fecundity.csv")
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
a=1:100
fec<- lapply(a, function(x)
{
  lower_ls<-age_1_length(n=n, mu=mu_l_1, sd=sd_l_1, min=min_l_1, max=max_l_1)
  VBparams<- growth_params(n=n, mu_ln_Linf=ln_Linf_mu, mu_ln_k=ln_k_mu, 
                           vcv=vcv)
  # max(VBparams[[1]])  ##TOO LARGE OF FISH PRODUCED... NEW MODEL FIT?... 
  ##WHY WAS LOG USED FOR THE PARAMETERS???
  lengths<- length_at_age(age=x, length_at_age_1=lower_ls, Linf=VBparams[[1]],
                          k=VBparams[[2]])
  mn_lgth<- mean(lengths)
  med_lgth<- median(lengths)
  lgth_1400_plus<- length(which(lengths>=1400))/length(lengths)
  fecundity<- eggs(fork_length=lengths, a=intrcpt, b=slp, dispersion_param=disp)
  mn_eggs<- mean(fecundity)
  med_eggs<- median(fecundity) 
  return(list(mean_eggs=mn_eggs, median_eggs=med_eggs, 
              mean_length=mn_lgth, median_length=med_lgth,
              proportion_1400_plus=lgth_1400_plus))
})
fecundity<- data.frame(Age=a, 
                       Mean_Eggs_Produced=sapply(fec, "[[", 1),
                       Median_Eggs_Produced=sapply(fec, "[[", 2),
                       Mean_Length=sapply(fec, "[[", 3),
                       Median_Length=sapply(fec, "[[", 4),
                       Proportion_Length_1400plus=sapply(fec, "[[", 5))
write.csv(fecundity, "./baseline-parameters/fecundity_estimates_by_age_comparison_100.csv",
          row.names = FALSE)

# MEAN FECUNDITY INCREASES LINEARLY WITH AGE AND IS CLOSE TO 100,000 BY AGE 60 
# THIS MAY BE EXPECTED BASED ON THE MATH--CHECK!
# DO WE EXPECT THIS IN REALITY?
# MEDIAN FECUNDITY INCREASES WITH SLIGHT DIMINISHING RETURNS AND IS MUCH LOWER

fecundity<- read.csv("./baseline-parameters/fecundity_estimates_by_age_100.csv")
plot(fecundity$Age, fecundity$Mean_Eggs_Produced/1000, xlab="Age (Years)", 
     ylab="Thousands of Eggs Produced Per Female", pch=19)



# ## CHECK VB_PARAMS
# VBparams<- growth_params(n=n, mu_ln_Linf=ln_Linf_mu, mu_ln_k=ln_k_mu, 
#                          vcv=vcv)
# length(which(VBparams[[1]]<1400))/length(VBparams[[1]])
# # 0.589312
# indx<- which(VBparams[[1]]>1400)
# a<- -1/(VBparams[[2]][indx])*log((VBparams[[1]][indx]-1400)/(VBparams[[1]][indx]-200))+1
# min(a)
# # 22.83147
# max(a)
# # 467.6466
# length(which(a<60))/length(a)
# # 0.7228918
# a_50<- -1/(VBparams[[2]][indx])*log((VBparams[[1]][indx]-1400)/(VBparams[[1]][indx]-50))+1
# min(a_50)
# # 24.50924
# a_350<- -1/(VBparams[[2]][indx])*log((VBparams[[1]][indx]-1400)/(VBparams[[1]][indx]-350))+1
# min(a_350)
# # 20.98657
# 
# 
# lower_ls<-age_1_length(n=n, mu=mu_l_1, sd=sd_l_1, min=min_l_1, max=max_l_1)
# 
# a=1:60
# lengths<- lapply(a, function(x)
# {
#   out<- length_at_age(age=x, length_at_age_1=lower_ls, Linf=VBparams[[1]],
#                           k=VBparams[[2]])
#   return(out)
# })
# maxL<- sapply(1:length(lengths), function(x){max(lengths[[x]])})
# perc<- sapply(1:length(lengths), function(x)
# {
#   length(which(lengths[[x]]>=1400))/length(lengths[[x]])
# })
# mn_lgth<- sapply(1:length(lengths), function(x){mean(lengths[[x]])})
# med_lgth<- sapply(1:length(lengths), function(x){median(lengths[[x]])})
# 
# fecundity<- lapply(1:length(a), function(x)
# {
#   out<- eggs(fork_length=lengths[[x]], a=intrcpt, b=slp, dispersion_param=disp)
#   return(out)
# })
# mn_fec<- sapply(1:length(fecundity), function(x){mean(fecundity[[x]])})
# med_fec<- sapply(1:length(fecundity), function(x){median(fecundity[[x]])})
# 
# 
# plot(a,mn_fec)
# points(a, med_fec, col="blue")
# max(mn_fec)
# max(med_fec)


# ##### REPEAT FOR LOWER RIVER
# # LOWER RIVER VON BERTALANFFY GROWTH PARAMETER DISTRIBUTION
# ln_Linf_muL<-6.982160
# ln_k_muL<- -2.382711
# vcvL<- matrix(c(0.0894,-0.1327,-0.1327,0.3179),
#               nrow=2, ncol=2, byrow=TRUE)
# 
# # LOWER RIVER LENGTH-FECUNDITY RELATIONSHIP
# ## FIT MODEL GIVEN DATA (FIT 7 USED IN POP MODEL)
# ## VERIFY ALL ASSUMPTIONS MET
# fitL<-glmer(EGGS~fl_std+(1|id),dat[which(dat$BASIN=="Lower"),],
#            family=poisson(link="log"))
# intrcptL<- unname(fixef(fitL)[1])
# slpL<- unname(fixef(fitL)[2])
# dispL<- (sd(unlist(ranef(fitL)$id))*(length(unlist(ranef(fitL)$id))-1)/length(unlist(ranef(fitL)$id))
#         +sd(unlist(ranef(fitL)$id)))/2
# rm(fitL)
# 
# ## SIMULATE FECUNDITY BY AGE
# aL=1:41
# fecL<- lapply(aL, function(x)
# {
#   lower_ls<-age_1_length(n=n, mu=mu_l_1, sd=sd_l_1, min=min_l_1, max=max_l_1)
#   VBparams<- growth_params(n=n, mu_ln_Linf=ln_Linf_muL, mu_ln_k=ln_k_muL,
#                            vcv=vcvL)
#   # max(VBparams[[1]])  ##TOO LARGE OF FISH PRODUCED... NEW MODEL FIT?...
#   ##WHY WAS LOG USED FOR THE PARAMETERS???
#   lengths<- length_at_age(age=x, length_at_age_1=lower_ls, Linf=VBparams[[1]],
#                           k=VBparams[[2]])
#   mn_lgth<- mean(lengths)
#   med_lgth<- median(lengths)
#   lgth_1400_plus<- length(which(lengths>=1400))/length(lengths)
#   fecundity<- eggs(fork_length=lengths, a=intrcptL, b=slpL, dispersion_param=dispL)
#   mn_eggs<- mean(fecundity)
#   med_eggs<- median(fecundity)
#   return(list(mean_eggs=mn_eggs, median_eggs=med_eggs,
#               mean_length=mn_lgth, median_length=med_lgth,
#               proportion_1400_plus=lgth_1400_plus))
# })
# fecundityL<- data.frame(Age=aL,
#                        Mean_Eggs_Produced=sapply(fecL, "[[", 1),
#                        Median_Eggs_Produced=sapply(fecL, "[[", 2),
#                        Mean_Length=sapply(fecL, "[[", 3),
#                        Median_Length=sapply(fecL, "[[", 4),
#                        Proportion_Length_1400plus=sapply(fecL, "[[", 5))
# 
# plot(fecundity$Age, fecundity$Mean_Eggs_Produced/1000, xlab="Age (Years)",
#      ylab="Thousands of Eggs Produced Per Female", pch=17, ylim=c(0,100))
# points(fecundityL$Age, fecundityL$Mean_Eggs_Produced/1000,pch=19)
# legend("topleft", c("UMOR", "LMOR"), pch=c(17,19), bty="n")
