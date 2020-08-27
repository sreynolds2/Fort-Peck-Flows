


# OBTAIN STANDARD ANNUAL TEMPERATURE-RETENTION DATA
## PULL ANNUAL TEMPERATURE DATA
temps<- read.csv("./dat/Observed_Annual_Temperature_Class.csv")
### SUBSET TO THOSE DATES RAN IN DEIS
temps<- subset(temps, Year %in% 1930:2012)
# OBTAIN STANDARD RETENTION DATA
### PULL RETENTIONS UNDER STANDARD DEVELOPMENT
### I.E., NEW DEVELOPMENT MODEL, 0.9 DRIFT, ABOVE ANOXIC ZONE
ret<- read.csv("./output/New_9_above_Retentions.csv",
               stringsAsFactors = FALSE)
ret$Hatch_Date<- as.Date(ret$Hatch_Date)
ret[which(is.na(ret$Alt)),]$Alt<- "NoAct"
### PULL STANDARD SPAWN DATES
spDts<- read.csv("./output/Spawn_Dates_Summary.csv",
                 stringsAsFactors = FALSE)
spDts<- spDts[,1:4]
spDts<- subset(spDts, Standard!="Fail")
spDts$Spawn_Date<- as.Date(paste0(spDts$Year, "-", spDts$Standard))
#### GET ALTERNATIVES TO MATCH FORMAT OF RETENTION FILE
spDts[which(spDts$Flow_Scenario=="Alt1"),]$Flow_Scenario<- "Alt.1"
alt<- unlist(lapply(strsplit(spDts$Flow_Scenario, "\\."), "[[", 2))
alt[which(alt=="Act")]<-"NoAct"
spDts$Alt<- alt
rm(alt)
#### MODIFY SPAWN DATES IN HIGH FLOW YEARS DUE TO LACK OF HEC-RAS RUNS
spDts[which(spDts$Year==1975),]$Spawn_Date<- "1975-07-02"
spDts[which(spDts$Year==1976),]$Spawn_Date<- "1976-06-14"
spDts[which(spDts$Year==1997 &
              spDts$Alt %in% c("1", "1a", "2", "2b", "NoAct")),]$Spawn_Date<- "1997-06-25"
spDts[which(spDts$Year==1997 &
              spDts$Alt %in% c("1b", "2a")),]$Spawn_Date<- "1997-06-30"
#### ADD HATCH DATE 7 DAYS LATER
spDts$Hatch_Date<- spDts$Spawn_Date+7
spDts<- spDts[,c("Alt", "Year", "Weather_Pattern", "Spawn_Date", "Hatch_Date")]
### SUBSET RETENTION DATA TO STANDARD HATCH DATES
dat<- merge(spDts, ret, by=c("Alt", "Year", "Hatch_Date"), all.x=TRUE)
rm(ret, spDts)
# missing<- dat[which(is.na(dat$Retention)),]
  ### 1997-07-07 is MISSING
#### USE AVERAGE RETENTION FOR GIVEN YEAR FOR NOW
dat[which(is.na(dat$Retention)),]$Retention<-
  mean(dat[which(dat$Year==1997),]$Retention, na.rm = TRUE)
dat$Spawn<- 1
## COMBINE RETENTION DATA AND TEMPERATURE DATA
### EXPAND TEMPERATURE DATA FOR EACH ALTERNATIVE
tmp<- merge(temps, data.frame(Alt=c("1", "1a", "1b", "2", "2a", "2b",
                                    "NoAct")))
dat<- merge(tmp, dat, by.x=c("Year", "Weather", "Alt"),
            by.y=c("Year", "Weather_Pattern", "Alt"), all=TRUE)
rm(tmp)
dat[which(is.na(dat$Spawn)),]$Spawn<- 0
dat[which(is.na(dat$Retention)),]$Retention<- 0
## CLEAN DATA AND REORDER
dat$Develop_Mod<- "New"
dat$Drift_Mod<- 0.9
dat$anoxic_layer<- FALSE
dat<- dat[,c("Alt", "Year", "Weather", "Spawn", "Retention", "Hatch_Date",
             "Spawn_Date", "Develop_Mod", "Drift_Mod", "anoxic_layer")]
dat<- dat[order(dat$Alt, dat$Year),]
write.csv(dat,
          "./output/_stochastic/Standard_Annual_Retentions_by_Alt.csv",
          row.names = FALSE)
#dat<- read.csv("./output/_stochastic/Standard_Annual_Retentions_by_Alt.csv", 
#               stringsAsFactors = FALSE)

# PULL TEMPERATURE TRANSITION MATRIX
trans<- read.csv("./output/_stochastic/Temperature_Transition_Matrix.csv",
                 row.names = 1)

# SIMULATE POPULATION TRAJECTORIES
yrs<- 10000 
reps<- 5
## GENERATE BASELINE POPULATION TRANSITION MATRIX
A_base<- matrix(0,60,60)
### SURVIVAL VALUES
A_base[cbind(2:60,1:59)]<- c(0.64, 0.69, 0.72, 0.76, 0.79, 0.82, 0.84, 
                             0.86, 0.88, 0.895, 0.91, 0.92, 0.93, 
                             0.935, rep(0.94, 45))
### FERTILITY VALUES WITH FULL SPAWNING AND RETENTION
psi<- c(0.0009110512, 0.0015615720, 0.0045672949, 0.0122266340, 
        0.0319099933, 0.0784642192, 0.1673188663, 0.2751973410, 
        0.3302198159, 0.3323092533, 0.3338885331, 0.3379265874, 
        0.3391453206, 0.3415561692, 0.3372606143, 0.3394286578, 
        0.3392222014, 0.3389512062, 0.3391677126, 0.3389911381, 
        0.3391178293, 0.3390692029, 0.3390652542, 0.3390825082, 
        0.3390692824, 0.3390768185, 0.3390735233, 0.3390740041,
        0.3390747888, 0.3390739004, 0.3390744560, 0.3390742164, 
        0.3390742673, 0.3390743005, 0.3390742492, 0.3390742871, 
        0.3390742685, 0.3390742736, 0.3390742749, 0.3390742719, 
        0.3390742744, 0.3390742730, 0.3390742735, 0.3390742735, 
        0.3390742733, 0.3390742735, 0.3390742734, 0.3390742734, 
        0.3390742734, 0.3390742734, 0.3390742734, 0.3390742734, 
        0.3390742734)
eggs<- c(17858.32, 19196.99, 20567.28, 21929.43, 23349.27, 
         24775.18, 26243.11, 27685.86, 29146.68, 30671.10, 32132.68, 
         33675.72, 35201.90, 36758.49, 38313.39, 39869.35, 41409.45, 
         42992.59, 44535.03, 46151.91, 47672.25, 49359.17, 50889.91, 
         52537.71, 54138.78, 55773.74, 57365.43, 58985.37, 60526.52, 
         62200.39, 63870.55, 65417.74, 67086.64, 68720.32, 70346.88, 
         71994.93, 73619.97, 75342.93, 76795.55, 78508.78, 80057.36, 
         81574.96, 83282.14, 85053.37, 86523.37, 88186.36, 89790.52, 
         91420.39, 92923.74, 94449.11, 96010.70, 97906.26, 99150.91)
sexratio<- 0.32
phi0_MR<- 0.000075
A_base[1,8:60] <- psi*eggs*sexratio*phi0_MR
rm(psi, eggs, sexratio, phi0_MR)

## SIMULATE TEMPERATURE DATA
temp_freq<- read.csv("./output/_stochastic/Temperature_Frequency.csv")
W<- matrix(0, nrow=yrs, ncol=reps)
W[1,]<- sample(c("Low", "Median", "High"), reps, replace=TRUE, 
               prob=temp_freq$Frequency/sum(temp_freq$Frequency))
for(j in 1:reps)
{
  for(i in 2:yrs)
  {
    W[i,j]<- sample(c("Low", "Median", "High"), 1, 
                    prob=trans[,W[i-1,j]])
  }
}
write.csv(W, "./output/_stochastic/Sample_Temp_Class_Round_1.csv", 
          row.names = FALSE)
## SIMULATE ENVIRONMENT DATA BY CHOOSING A YEAR WITH GIVEN TEMPERATURE
omega<- sapply(1:length(W), function(i)
{
  sample(temps[which(temps$Weather==c(W[i])),]$Year,1)
})
omega<- matrix(omega, nrow=yrs, ncol=reps, byrow = FALSE)
write.csv(omega, "./output/_stochastic/Sample_Paths_Round_1.csv", 
          row.names = FALSE)
#test<- read.csv("./output/_stochastic/Sample_Paths_Round_1.csv")
#test<- unname(as.matrix(test))
rm(W, i, j)
## SIMULATE POPULATION STRUCTURE OVER THE YEARS FOR EACH ALTERNATIVE
alts<- c("1", "1a", "1b", "2", "2a", "2b", "NoAct")
### PULL AVERAGE MATRIX POPULATION STRUCTURE
AvgMC_dat<- readRDS("./output/_stochastic/Average_Matrix_Data_MC.rds")
Y0<- AvgMC_dat$`1`$stable_age
out<- lapply(alts, function(y)
{
  # ## USE INITIAL AND SUBSEQUENT POPULATION STRUCTURE TO ESTIMATE
  # ## STOCHASTIC GROWTH RATE
  # Y0<- N0/sum(N0)
  # logLtau<- sapply(1:reps, function(w)
  # {
  #   Y_t<- A_list[[omega[w,1]]]%*%Y0/sum(A_list[[omega[w,1]]]%*%Y0)
  #   loglt_sum<- log(sum(A_list[[omega[w,1]]]%*%Y0))
  #   for(i in 2:tau)
  #   {
  #     Y_t<- A_list[[omega[w,i]]]%*%Y_t/sum(A_list[[omega[w,i]]]%*%Y_t)
  #     loglt_sum<- log(sum(A_list[[omega[w,i]]]%*%Y_t))+loglt_sum
  #   }
  #   return(loglt_sum)
  # })
  # l_ap2b<- exp(mean(logLtau/tau))
  #
  #
  # ### TEST CONVERGENCE
  # #### CHECK log(Ltau2) IS APPROXIMATELY NORMALLY DISTRIBUTED
  # hist(logLtau)
  # qqnorm(logLtau)
  # qqline(logLtau, col="red", lty=3)
  
  #### CHECK THAT THE QUANTILES OF EARLIER AND LATER RUNS ARE SIMILAR
  #### AND DISTRIBUTION IS NOT CHANGING -- MAY NEED LARGER TAU
  ##### RERUN ABOVE BUT STORE LOG OF ANNUAL GROWTH RATES 
  tmp<- subset(dat, Alt==y)
  logl_t<- sapply(1:reps, function(w)
  {
    Y_t<- Y0
    loglt<- NULL
    for(i in 1:665)#yrs)
    {
      if(tmp[which(tmp$Year==omega[i,w]),]$Spawn==0)
      {
        A_t<- A_base
        A_t[1,]<- 0
      }
      if(tmp[which(tmp$Year==omega[i,w]),]$Spawn==1)
      {
        A_t<- A_base
        A_t[1,8:60]<- A_t[1,8:60]*0.5*tmp[which(tmp$Year==omega[i,w]),]$Retention
      }
      Y_t<- A_t%*%Y_t/sum(A_t%*%Y_t)
      loglt<- c(loglt, log(sum(A_t%*%Y_t)))
        #CHECK ON SUM ABOVE; NEED CODE FOR WHEN POPULATION GOES EXTINCT
    }
    return(loglt)
  })
  write.csv(logl_t, paste0("./output/_stochastic/Alt_", y, 
                           "_Loglt_Round_1.csv"), row.names = FALSE)
  return(logl_t)
})
names(out)<- alts
saveRDS(out, "./output/_stochastic/Loglt_Round_1.rds")

# ALT 1
## LONG-TERM LAMBDA
logLtau<- colSums(out[[1]])
l<- exp(mean(logLtau/yrs))
# ##### COMPARE QUANTILES FROM t=4900 and t=5000
# qqplot(colSums(logl_t[1:4900,]), colSums(logl_t))
# abline(0,1, col="red", lty=3)
# 
# ##### COMPARE QUANTILES FROM t=4500 and t=5000
# qqplot(colSums(logl_t[1:4500,]), colSums(logl_t))
# abline(0,1, col="red", lty=3)
# 
# ##### COMPARE QUANTILES FROM t=4000 and t=1000
# qqplot(colSums(logl_t[1:4000,]), colSums(logl_t))
# abline(0,1, col="red", lty=3)
# 
# ##### NOTE THAT THE GROWTH RATE OF A GIVEN RUN HAS CONVERGED
# x<- sample(1:M, 1)
# plot(1:tau, cumsum(logl_t[,x])/1:tau, type = "l")
# abline(sum(logl_t[,x])/tau,0,col="red", lty=3)
# 
# ##### WHAT ABOUT LOG GROWTH?
# ## SAMPLE PATH LENGTH
# tau2<- 100000
# ## GENERATE SAMPLE PATHS
# omega2<- sample(1:3,1)
# for(i in 2:tau2)
# {
#   omega2<- c(omega2,sample(1:3, 1, prob=P[,omega2[i-1]]))
# }
# 
# ## METHOD 2B WITH logL_t STORED AND LOG GROWTH PLOT
# Y_t<- A_list[[omega2[1]]]%*%Y0/sum(A_list[[omega2[1]]]%*%Y0)
# logL_t<- log(sum(A_list[[omega2[1]]]%*%Y0))
# for(i in 2:tau2)
# {
#   Y_t<- A_list[[omega2[i]]]%*%Y_t/sum(A_list[[omega2[i]]]%*%Y_t)
#   logL_t<- c(logL_t, log(sum(A_list[[omega2[i]]]%*%Y_t))+logL_t[i-1])
# }
# plot(1:tau2, logL_t, type = "l")
# points(1:tau2, logL_t[1]+logL_t[tau2]/tau2*1:tau2, type="l", col="red", lty=3)
# # SLOPE AT LATER TIMES SHOULD BE SIMILAR TO SLOPE OF DOTTED RED LINE
# # GIVEN ENOUGH TIME HAS PASSED
