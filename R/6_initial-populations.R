
library(plyr)

# GENERATE INITIAL POPULATION
init_pop<- function(inputs=NULL,
                    type=NULL) # "Uniform" OR "2020_est"
{
  inps<- inputs
  if(type=="Uniform")
  {
    ## 2020 AMCR MEDIAN POP EST
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
    inps$N0_type<- type
  }
  if(type=="2020_est")
  {
    
  }
  return(inps)
}

# GENERATE BASELINE POPULATION TRANSITION MATRIX
leslie<- function(inputs=NULL,
                  changes=list())
{
  inps<- inputs
  if(length(changes!=0))
  {
    for(i in 1:length(changes))
    {
      inps[[match(names(changes)[i], names(inps))]]<- changes[[i]]
    }
  }
  A_base<- matrix(0,inps$max_age,inps$max_age)
  ## SURVIVAL VALUES
  A_base[cbind(2:inps$max_age,1:(inps$max_age-1))]<- inps$phi
  ## FERTILITY VALUES WITH FULL SPAWNING AND RETENTION
  A_base[1,] <- inps$psi*inps$eggs*inps$probF*inps$phi0_MR
  ## ADD TO INPUTS
  inps$A_base<- A_base
  return(inps)
}

project_environ<- function(inputs=NULL,
                           temp_data=NULL,
                           transitions=NULL,
                           temp_freq_data=NULL,
                           years=200,
                           reps=5000,
                           id=1)
{
  temps<- temp_data
  trans<- transitions
  temp_freq<- temp_freq_data
  ## SIMULATE TEMPERATURE DATA
  W<- matrix(0, nrow=years, ncol=reps)
  W[1,]<- sample(c("Low", "Median", "High"), reps, replace=TRUE, 
                 prob=temp_freq$Frequency/sum(temp_freq$Frequency))
  for(j in 1:reps)
  {
    for(i in 2:years)
    {
      W[i,j]<- sample(c("Low", "Median", "High"), 1, 
                      prob=trans[,W[i-1,j]])
    }
  }
  write.csv(W, paste0("./output/_stochastic/Sample_Temp_Class_", id, ".csv"), 
            row.names = FALSE)
  ## SIMULATE ENVIRONMENT DATA BY CHOOSING A YEAR WITH GIVEN TEMPERATURE
  omega<- sapply(1:length(W), function(i)
  {
    sample(temps[which(temps$Weather==c(W[i])),]$Year,1)
  })
  omega<- matrix(omega, nrow=years, ncol=reps, byrow = FALSE)
  write.csv(omega, paste0("./output/_stochastic/Sample_Paths_", id, ".csv"), 
            row.names = FALSE)
  inputs$environment<- list(temps=temps, transitions=trans, temp_freq=temp_freq, 
                            environ_paths=omega, id=id)
  return(inputs)
}

project_pop<- function(inputs=NULL,
                       retention_data=NULL,
                       N0_type="Uniform",
                       adjustments=NULL,
                       years=200,
                       reps=5000,
                       alts=c("1", "1a", "1b", "2", "2a", "2b", "NoAct"),
                       param_id=1)
{
  dat<- retention_data
  # ERROR CHECK
  omega<- inputs$environment$environ_paths 
  if(years>nrow(omega))
  {
    return(print("Environment data does not contain enough years for the given projection."))
  }
  if(reps>ncol(omega))
  {
    return(print("Environment data does not contain enough replicates for the given projection."))
  }
  # SAVE INPUTS
  inps<- init_pop(inputs, N0_type)
  inps<- leslie(inputs = inps, 
                changes = adjustments)
  inps$yrs<- years
  inps$reps<- reps
  inps$param_id<- param_id
  inps$ret_dat<- dat
  # SIMULATE POPULATION TRAJECTORIES
  out<- lapply(alts, function(y)
  {
    ##### STORE POPULATION SIZES 
    tmp<- subset(dat, Flow_Scenario==y)
    Nt<- sapply(1:reps, function(w)
    {
      N_t<- inps$N0
      N_tot<- sum(N_t)
      for(i in 1:years)
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
        N_t<- A_t%*%N_t
        N_tot<- c(N_tot, sum(N_t))
      }
      return(N_tot)
    })
    write.csv(Nt, paste0("./output/_stochastic/Alt_", y, "_Ntot_",
                         inps$environment$id, "-", inps$param_id, ".csv"), 
              row.names = FALSE)
    return(Nt)
  })
  names(out)<- alts
  out$inputs<- inps
  saveRDS(out, paste0("./output/_stochastic/Ntot_", inps$environment$id, 
                      "-", inps$param_id, ".rds"))
  return(out)
}

### COMPUTE FRACTION EXTINCT

reps<- out$inputs$reps
yrs<- out$inputs$yrs
tmp<- out$`2b`
# lambda<- t(sapply(1:yrs, function(i){tmp[i+1,]/tmp[i,]}))
max(tmp[151,])

# OBTAIN INPUT DATA
source("./R/0_default-parameters.r")
                           
# OBTAIN TEMPERATURE, RETENTION, AND SPAWNING DATA
## PULL ANNUAL TEMPERATURE DATA
temps<- read.csv("./dat/Observed_Annual_Temperature_Class.csv")
### SUBSET TO THOSE DATES RAN IN DEIS
temps<- subset(temps, Year %in% 1930:2012)
## PULL STANDARD RETENTION DATA
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
## PULL TEMPERATURE TRANSITION MATRIX
trans<- read.csv("./output/_stochastic/Temperature_Transition_Matrix.csv",
                 row.names = 1)
## PULL TEMPERATURE FREQUENCY TABLE
temp_freq<- read.csv("./output/_stochastic/Temperature_Frequency.csv")


# SIMULATE ENVIRONMENT
inps<- project_environ(inputs = inputs,
                       temp_data = temps,
                       transitions = trans,
                       temp_freq_data = temp_freq,
                       id=1)

# SIMULATE POPULATION REPS
phi0MR<- c(0.000075, 0.95*0.000075, 1.05*0.000075, 
           seq(0.0002, 0.002, 0.0002))
params<- data.frame(phi0_MR=phi0MR, param_id=1:length(phi0MR))

ptm<-proc.time()
phi0_test<- lapply(params$param_id[2:nrow(params)], function(x)
{
  test<- project_pop(inputs = inps,
                     retention_data = dat,
                     adjustments = list(phi0_MR=params$phi0_MR[x]),
                     years=200,
                     reps=5000,
                     param_id=params$param_id[x])
  return(test)
})
tot<-(proc.time()-ptm)[3]/60
phi0_test$tot<- tot
tot

ptm<-proc.time()
test<- project_pop(inputs = inps,
                   retention_data = dat,
                   adjustments = NULL,
                   years=200,
                   reps=5000,
                   param_id=1)
tot<-(proc.time()-ptm)[3]/60
test$tot<- tot
tot

params<- rbind.fill(params, data.frame(max_age=c(99, 101), 
                                       param_id=1:2+max(params$param_id)))


library(parallel)
numCores<- detectCores()
cl<- makeCluster(numCores)
clusterExport(cl, c("inps", "dat", "init_pop", "leslie", "project_pop"))
ptm<-proc.time()
phi<- lapply(1:length(inps$phi), function(x)
{
  inps2<- inps
  inps2$phi[x]<- 0.95*inps$phi[x]
  test1<- project_pop(inputs = inps2,
                     retention_data = dat,
                     adjustments = list(),
                     years=200,
                     reps=5000,
                     param_id=16+2*(x-1))
  inps2$phi[x]<- 1.05*inps$phi[x]
  test2<- project_pop(inputs = inps2,
                      retention_data = dat,
                      adjustments = list(),
                      years=200,
                      reps=5000,
                      param_id=17+2*(x-1))
  return(list(decrease=test1, increase=test2))
})
tot<-(proc.time()-ptm)[3]/60
phi$tot<- tot
tot
stopCluster(cl)

# out<- lapply(alts, function(y)
# {
#   ##### STORE LOG OF ANNUAL GROWTH RATES 
#   tmp<- subset(dat, Flow_Scenario==y)
#   logl_t<- sapply(1:inps$reps, function(w)
#   {
#     Y_t<- inps$N0/sum(inps$N0)
#     loglt<- NULL
#     for(i in 1:inps$yrs)
#     {
#       if(tmp[which(tmp$Year==omega[i,w]),]$Spawn==0)
#       {
#         A_t<- inps$A_base
#         A_t[1,]<- 0
#       }
#       if(tmp[which(tmp$Year==omega[i,w]),]$Spawn==1)
#       {
#         A_t<- inps$A_base
#         A_t[1,]<- A_t[1,]*0.5*tmp[which(tmp$Year==omega[i,w]),]$Retention
#       }
#       ## HANDLE POPULATION GOING EXTINCT
#       if(sum(A_t%*%Y_t)==0)
#       {
#         loglt<- c(loglt, -Inf)
#         Y_t<- rep(0,inps$max_age)
#       }
#       ## HANDLE PERSISTING POPULATION
#       if(sum(A_t%*%Y_t)!=0)
#       {
#         loglt<- c(loglt, log(sum(A_t%*%Y_t)))
#         Y_t<- A_t%*%Y_t/sum(A_t%*%Y_t)
#       }
#     }
#     return(loglt)
#   })
#   write.csv(logl_t, paste0("./output/_stochastic/Alt_", y, 
#                            "_Loglt_Test.csv"), row.names = FALSE)
#   return(logl_t)
# })
# names(out)<- alts
# out$inputs<- inps
# saveRDS(out, "./output/_stochastic/Loglt_Test.rds")






