
source("./R/0_default-parameters.r")

# GENERATE INITIAL POPULATION
## 2020 AMCR MEDIAN POP EST
inps<- inputs
inps$N_H<- 11853
inps$N_W<- 505
inps$sexratio_H<- 0.5
inps$sexratio_W<- 0.32
  # SOURCE: THE 40:85 ADULT F:M RATIO ESTIMATED BY JAEGER ET AL. (2009) 
  # IN THE 2017 AM REPORT (TABLE 3-1) 
N<- ceiling(inps$N_H*inps$sexratio_H+inps$N_W*inps$sexratio_W)
N0<- rep(floor(N/inps$max_age), inps$max_age)
N0<- N0+c(rep(1, N-sum(N0)), rep(0, inps$max_age-N+sum(N0)))
inps$N0<- N0
rm(N,N0)

# GENERATE BASELINE POPULATION TRANSITION MATRIX
A_base<- matrix(0,inps$max_age,inps$max_age)
## SURVIVAL VALUES
A_base[cbind(2:inps$max_age,1:(inps$max_age-1))]<- inps$phi
## FERTILITY VALUES WITH FULL SPAWNING AND RETENTION
A_base[1,] <- inps$psi*inps$eggs*inps$probF*inps$phi0_MR
## ADD TO INPUTS
inps$A_base<- A_base
rm(A_base)

# OBTAIN TEMPERATURE, RETENTION, AND SPAWNING DATA
## PULL ANNUAL TEMPERATURE DATA
temps<- read.csv("./dat/Observed_Annual_Temperature_Class.csv")
### SUBSET TO THOSE DATES RAN IN DEIS
temps<- subset(temps, Year %in% 1930:2012)
### SAVE TO INPUTS
inps$temps<- temps
rm(temps)
# ## PULL STANDARD RETENTION DATA
# dat<- read.csv("./output/long-term_lambda_Standard_by_Year_DSM_no_zeros_new.csv",
#                stringsAsFactors = FALSE)
# ## ADD IN SPAWNING BINARY (1=SPAWN)
# dat$Spawn<- 1
# ## COMBINE TEMPERATURE AND RETENTION DATA
# ### EXPAND TEMPERATURE DATA FOR EACH ALTERNATIVE
# tmp<- merge(temps, data.frame(Flow_Scenario=c("1", "1a", "1b", "2", "2a", "2b",
#                                     "NoAct")))
# dat<- merge(tmp, dat, by.x=c("Year", "Weather", "Flow_Scenario"),
#             by.y=c("Year", "Weather_Pattern", "Flow_Scenario"), all=TRUE)
# rm(tmp)
# dat[which(is.na(dat$Spawn)),]$Spawn<- 0
# dat[which(is.na(dat$Retention)),]$Retention<- 0
# ## CLEAN DATA AND REORDER
# dat$Develop_Mod<- "New"
# dat$Drift_Mod<- 0.9
# dat$anoxic_layer<- FALSE
# names(dat)[match("Standard", names(dat))]<- "Spawn_Date"
# dat<- dat[,c("Flow_Scenario", "Year", "Weather", "Spawn", "Retention",
#              "Long_Term_Growth_Rate", "Hatch_Date", "Spawn_Date", "Develop_Mod", 
#              "Drift_Mod", "anoxic_layer")]
# dat<- dat[order(dat$Flow_Scenario, dat$Year),]
# write.csv(dat,
#           "./output/Standard_Annual_Retentions_by_Alt_new.csv",
#           row.names = FALSE)
dat<- read.csv("./output/Standard_Annual_Retentions_by_Alt_new.csv", 
               stringsAsFactors = FALSE)

# PULL TEMPERATURE TRANSITION MATRIX & SAVE TO INPUTS
trans<- read.csv("./output/_stochastic/Temperature_Transition_Matrix.csv",
                 row.names = 1)
inps$trans<- trans
rm(trans)

# SIMULATE POPULATION TRAJECTORIES
inps$yrs<- 150 
inps$reps<- 2000
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
write.csv(W, "./output/_stochastic/Sample_Temp_Class_Test.csv", 
          row.names = FALSE)
## SIMULATE ENVIRONMENT DATA BY CHOOSING A YEAR WITH GIVEN TEMPERATURE
omega<- sapply(1:length(W), function(i)
{
  sample(temps[which(temps$Weather==c(W[i])),]$Year,1)
})
omega<- matrix(omega, nrow=yrs, ncol=reps, byrow = FALSE)
write.csv(omega, "./output/_stochastic/Sample_Paths_Test.csv", 
          row.names = FALSE)
#omega<- read.csv("./output/_stochastic/Sample_Paths_Test.csv")
#omega<- unname(as.matrix(omega))
rm(W, i, j)
## SIMULATE POPULATION STRUCTURE OVER THE YEARS FOR EACH ALTERNATIVE
alts<- c("1", "1a", "1b", "2", "2a", "2b", "NoAct")
out<- lapply(alts, function(y)
{
  ##### STORE LOG OF ANNUAL GROWTH RATES 
  tmp<- subset(dat, Flow_Scenario==y)
  logl_t<- sapply(1:inps$reps, function(w)
  {
    N_t<- inps$N0
    loglt<- NULL
    for(i in 1:inps$yrs)
    {
      if(tmp[which(tmp$Year==omega[i,w]),]$Spawn==0)
      {
        A_t<- inps$A_base
        A_t[1,]<- 0
      }
      if(tmp[which(tmp$Year==omega[i,w]),]$Spawn==1)
      {
        A_t<- inps$A_base
        A_t[1,]<- A_t[1,]*0.5*tmp[which(tmp$Year==omega[i,w]),]$Retention
      }
      ## HANDLE POPULATION GOING EXTINCT
      if(sum(A_t%*%N_t)==0)
      {
        loglt<- c(loglt, -Inf)
        N_t<- rep(0,inps$max_age)
      }
      ## HANDLE PERSISTING POPULATION
      if(sum(A_t%*%N_t)!=0)
      {
        loglt<- c(loglt, log(sum(A_t%*%N_t)))
        N_t<- A_t%*%N_t/sum(A_t%*%N_t)
      }
    }
    return(loglt)
  })
  write.csv(logl_t, paste0("./output/_stochastic/Alt_", y, 
                           "_Loglt_Test.csv"), row.names = FALSE)
  return(logl_t)
})
names(out)<- alts
saveRDS(out, "./output/_stochastic/Loglt_Test.rds")
