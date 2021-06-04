
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
                       gamma=0.5,
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
          A_t[1,]<- A_t[1,]*gamma*tmp[which(tmp$Year==omega[i,w]),]$Retention
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

# dat<- readRDS("./output/_stochastic/Ntot_1-1.rds")
### COMPUTE FRACTION EXTINCT
pseudo_extinct<- function(pop_data=NULL,
                          threshold=50,
                          years=200,
                          reps=5000)
{
  # ERROR CHECK
  if(years>pop_data$inputs$yrs)
  {
    return(print("Population data does not contain enough years for the desired estimate."))
  }
  if(reps>pop_data$inputs$reps)
  {
    return(print("Population data does not contain enough replicates for the desired estimate."))
  }
  # COMPUTE FRACTION BELOW THRESHOLD FOR EACH ALTERNATIVE
  out<- lapply(1:7, function(x)
  {
      tmp<- pop_data[[x]]
      tmp<- tmp[,1:reps]
      yr<- sapply(1:reps, function(z)
      {
        val<- ifelse(all(tmp[,z]>=threshold),
                     years*10,
                     min(which(tmp[,z]<threshold)))
        return(val)
      })
      frac<- sapply(2:(years+1), function(y)
      {
        val<- length(which(yr<=y))/reps
      })
      return(list(extinction_yr=yr, frac_extinct=frac))
  })
  names(out)<-names(pop_data)[1:7]
  pop_data$extinction<-out
  pop_data$extinction$threshold<- threshold
  return(pop_data)
}

frac_extinct<- function(ext_data=NULL,
                        threshold=50,
                        years=200,
                        reps=5000)
{
  # ERROR CHECK
  if(threshold!=ext_data$extinction$threshold)
  {
    return(print("Extinction data is not for the given threshold."))
  }
  if(years>length(ext_data$extinction[[1]]$frac_extinct))
  {
    return(print("Extinction data does not contain enough years for the desired estimate."))
  }
  if(reps>length(ext_data$extinction[[1]]$extinction_yr))
  {
    return(print("Extinction data does not contain enough replicates for the desired estimate."))
  }
  # PULL DESIRED ESTIMATE FOR EACH ALTERNATIVE
  frac<- lapply(1:7, function(x)
  {
    tmp<- ext_data$extinction[[x]]
    val<- length(which(tmp$extinction_yr[1:reps]<=years))/reps
    return(val)
  })
  out<- data.frame(flow_scenario=names(ext_data$extinction)[1:7],
                   frac_extinct=unlist(frac),
                   param_id=ext_data$inputs$param_id)
  return(out)
}


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


# # SIMULATE ENVIRONMENT
# inps<- project_environ(inputs = inputs,
#                        temp_data = temps,
#                        transitions = trans,
#                        temp_freq_data = temp_freq,
#                        id=1)
getinps<- readRDS("./output/_stochastic/Ntot_1-1.rds")
inps<- inputs
inps$environment<- getinps$inputs$environment
rm(getinps)

# SIMULATE POPULATION REPS
## BASELINE & AGE-0 SURVIVAL GIVEN RETENTION
# phi0MR<- c(0.000075, 0.95*0.000075, 1.05*0.000075, 
#            seq(0.0002, 0.002, 0.0002))
# params<- data.frame(phi0_MR=phi0MR, param_id=1:length(phi0MR))
# 
# ptm<-proc.time()
# phi0_test<- lapply(params$param_id[2:nrow(params)], function(x)
# {
#   test<- project_pop(inputs = inps,
#                      retention_data = dat,
#                      gamma=0.5,
#                      adjustments = list(phi0_MR=params$phi0_MR[x]),
#                      years=200,
#                      reps=5000,
#                      param_id=params$param_id[x])
#   return(test)
# })
# tot<-(proc.time()-ptm)[3]/60
# phi0_test$tot<- tot
# tot

# MAXIMUM AGE
# params<- rbind.fill(params, data.frame(max_age=c(99, 101), 
#                                        param_id=1:2+max(params$param_id)))
# 
# inps2<- inps
# inps2$max_age<- 99
# inps2$phi<- inps$phi[1:(length(inps$phi)-1)]
# inps2$psi<- inps$psi[1:(length(inps$psi)-1)]
# inps2$eggs<- inps$eggs[1:(length(inps$eggs)-1)]
# 
# test1<- project_pop(inputs = inps2,
#                     retention_data = dat,
#                     gamma=0.5,
#                     adjustments = list(),
#                     years=200,
#                     reps=5000,
#                     param_id=14)
# 
# inps2<- inps
# inps2$max_age<- 101
# inps2$phi<- c(inps$phi, inps$phi[length(inps$phi)])
# inps2$psi<- c(inps$psi, inps$psi[length(inps$psi)])
# ## RUN EGG SIMULATION FOR 101 AND PULL NEW DATA
# e101<- read.csv("./baseline-parameters/fecundity_estimates_age_101.csv") 
# e101<- e101$Mean_Eggs_Produced
# inps2$eggs<- c(inps$eggs, e101)
# 
# test2<- project_pop(inputs = inps2,
#                     retention_data = dat,
#                     gamma=0.5,
#                     adjustments = list(),
#                     years=200,
#                     reps=5000,
#                     param_id=15)
# rm(inps2, test1, test2, e101)

# AGE-1+ SURVIVALS
# phi<- lapply(1:length(inps$phi), function(x)
# {
#   phi_a<- c(inps$phi[x]*0.95, inps$phi[x]*1.05)
#   age_id<- c(x,x)
#   param_id<- c((2*x-1):(2*x)+max(params$param_id))
#   out<- data.frame(phi=phi_a, age_id=age_id, param_id=param_id)
#   return(out)
# })
# phi<- do.call("rbind", phi)
# params<- rbind.fill(params, phi)
# rm(phi)

# library(parallel)
# numCores<- detectCores()
# cl<- makeCluster(numCores)
# clusterExport(cl, c("inps", "dat", "init_pop", "leslie", "project_pop"))
# ptm<-proc.time()
# phi<- parLapply(cl, 1:length(inps$phi), function(x)
# {
#   inps2<- inps
#   inps2$phi[x]<- 0.95*inps$phi[x]
#   test1<- project_pop(inputs = inps2,
#                      retention_data = dat,
#                      gamma=0.5,
#                      adjustments = list(),
#                      years=200,
#                      reps=5000,
#                      param_id=16+2*(x-1))
#   inps2$phi[x]<- 1.05*inps$phi[x]
#   test2<- project_pop(inputs = inps2,
#                       retention_data = dat,
#                       gamma=0.5,
#                       adjustments = list(),
#                       years=200,
#                       reps=5000,
#                       param_id=17+2*(x-1))
#   return(list(decrease=test1, increase=test2))
# })
# tot<-(proc.time()-ptm)[3]/60
# phi$tot<- tot
# tot
# stopCluster(cl)
# rm(cl)

# REPRODUCTIVE READINESS
# psi<- lapply(inps$mat$a_min:inps$max_age, function(x)
# {
#   psi_a<- c(inps$psi[x]*0.95, inps$psi[x]*1.05)
#   age_id<- c(x,x)
#   param_id<- c((2*(x-inps$mat$a_min+1)-1):(2*(x-inps$mat$a_min+1))+max(params$param_id))
#   out<- data.frame(psi=psi_a, age_id=age_id, param_id=param_id)
#   return(out)
# })
# psi<- do.call("rbind", psi)
# params<- rbind.fill(params, psi)
# rm(psi)

# library(parallel)
# numCores<- detectCores()
# cl<- makeCluster(numCores)
# clusterExport(cl, c("inps", "dat", "init_pop", "leslie", "project_pop"))
# ptm<-proc.time()
# invisible(parLapply(cl, 1:length(inps$psi),#c(31,50,69,87), 
#                     function(x)
# {
#   inps2<- inps
#   inps2$psi[x]<- 0.95*inps$psi[x]
#   test1<- project_pop(inputs = inps2,
#                       retention_data = dat,
#                       gamma=0.5,
#                       adjustments = list(),
#                       years=200,
#                       reps=5000,
#                       param_id=214+2*(x-14))
#   inps2$psi[x]<- 1.05*inps$psi[x]
#   test2<- project_pop(inputs = inps2,
#                       retention_data = dat,
#                       gamma=0.5,
#                       adjustments = list(),
#                       years=200,
#                       reps=5000,
#                       param_id=215+2*(x-14))
# }))
# tot<-(proc.time()-ptm)[3]/60
# tot
# stopCluster(cl)
# rm(cl)

# EGGS
# eggs<- lapply(inps$mat$a_min:inps$max_age, function(x)
# {
#   eggs_a<- c(inps$eggs[x]*0.95, inps$eggs[x]*1.05)
#   age_id<- c(x,x)
#   param_id<- c((2*(x-inps$mat$a_min+1)-1):(2*(x-inps$mat$a_min+1))+max(params$param_id))
#   out<- data.frame(eggs=eggs_a, age_id=age_id, param_id=param_id)
#   return(out)
# })
# eggs<- do.call("rbind", eggs)
# params<- rbind.fill(params, eggs)
# rm(eggs)

params<- read.csv("./output/_stochastic/sens_elas_vals.csv")
e_params<- params[which(!is.na(params$eggs)), c("eggs", "age_id", "param_id")]
library(parallel)
numCores<- detectCores()
cl<- makeCluster(numCores)
clusterExport(cl, c("inps", "dat", "e_params", "init_pop", "leslie", 
                    "project_pop"))
ptm<-proc.time()
eggs<- parLapply(cl, 1:nrow(e_params), function(x)
{
  inps2<- inps
  inps2$eggs[params$age_id[x]]<- params$eggs[x]
  test<- project_pop(inputs = inps2,
                      retention_data = dat,
                      gamma=0.5,
                      adjustments = list(),
                      years=200,
                      reps=5000,
                      param_id=params$param_id[x])
  return(test)
})
tot<-(proc.time()-ptm)[3]/60
tot
stopCluster(cl)
rm(cl, e_params)

# SEX RATIO
# r<- data.frame(probF=c(0.95*inps$probF, 1.05*inps$probF),
#                param_id=1:2+max(params$param_id))
# params<- rbind.fill(params, r)
# 
# library(parallel)
# cl<- makeCluster(2)
# clusterExport(cl, c("inps", "dat", "r", "init_pop", "leslie", 
#                     "project_pop"))
# invisible(parLapply(cl, 1:2, function(x)
# {
#   test<- project_pop(inputs = inps,
#                      retention_data = dat,
#                      gamma=0.5,
#                      adjustments = list(probF=r$probF[x]),
#                      years=200,
#                      reps=5000,
#                      param_id=r$param_id[x])
# }))
# stopCluster(cl)
# rm(cl, r)

# PROPORTION OF REPRODUCTIVELY READY FEMALES SPAWNING BELOW FT PECK
# g<- data.frame(gamma=c(0.95*0.5, 1.05*0.5),
#                param_id=1:2+max(params$param_id))
# params<- rbind.fill(params, g)
# 
# library(parallel)
# cl<- makeCluster(2)
# clusterExport(cl, c("inps", "dat", "g", "init_pop", "leslie", 
#                     "project_pop"))
# invisible(parLapply(cl, 1:2, function(x)
# {
#   test<- project_pop(inputs = inps,
#                      retention_data = dat,
#                      gamma=g$gamma[x],
#                      adjustments = list(),
#                      years=200,
#                      reps=5000,
#                      param_id=g$param_id[x])
# }))
# stopCluster(cl)
# rm(cl, g)

# RETENTION
# ret<- data.frame(retention=c("Dec. 5%", "Inc. 5%"),
#                  param_id=1:2+max(params$param_id))
# params<- rbind.fill(params, ret)

# dec_dat<- dat
# dec_dat$Retention<- 0.95*dec_dat$Retention
# dec<- project_pop(inputs = inps,
#                   retention_data = dec_dat,
#                   gamma=0.5,
#                   adjustments = list(),
#                   years=200,
#                   reps=5000,
#                   param_id=ret$param_id[1])
# inc_dat<- dat
# inc_dat$Retention<- 1.05*inc_dat$Retention
# inc<- project_pop(inputs = inps,
#                   retention_data = inc_dat,
#                   gamma=0.5,
#                   adjustments = list(),
#                   years=200,
#                   reps=5000,
#                   param_id=ret$param_id[2])
# rm(ret, dec_dat, dec, inc_dat, inc)


# SAVE PARAMETER VALUES FOR SENSITIVITY-ELASTICITY ANALYSES
# params<- params[order(params$param_id), c(2,1,3:ncol(params))]
# write.csv(params, "./output/_stochastic/sens_elas_vals.csv")

## EXTINCTION ANALYSES
library(parallel)
numCores<- detectCores()
cl<- makeCluster(numCores)
clusterExport(cl, c("pseudo_extinct"))
ptm<-proc.time()
invisible(parLapply(cl, c(84:171,212,213), function(p)
{
  dat<- readRDS(paste0("./output/_stochastic/Ntot_1-", p, ".rds"))
  out<-pseudo_extinct(pop_data = dat, threshold = 50,
                      years=200, reps=5000)
  saveRDS(out, paste0("./output/_stochastic/Full_Data_1-", p, ".rds"))
}))
tot<-(proc.time()-ptm)[3]/60
tot
stopCluster(cl)
rm(cl)

library(parallel)
numCores<- detectCores()
cl<- makeCluster(numCores)
clusterExport(cl, c("frac_extinct"))
ptm<-proc.time()
explore_ext<- parLapply(cl, 1:19, function(p)
{
  dat<- readRDS(paste0("./output/_stochastic/Full_Data_1-", p, ".rds"))
  yr_test<- lapply(seq(75, 200, 25), function(y)
  {
    rep_no_test<- lapply(seq(1000, 5000, 1000), function(r)
    {
      out<- frac_extinct(ext_data = dat, threshold = 50,
                         years=y, reps=r)
      out$reps<- r
      return(out)
    })
    rep_no_test<- do.call("rbind", rep_no_test)
    rep_no_test$years<- y
    return(rep_no_test)
  })
  yr_test<- do.call("rbind", yr_test)
  return(yr_test)
})
ext_test<- do.call("rbind", explore_ext)
tot<-(proc.time()-ptm)[3]/60
tot
stopCluster(cl)
rm(cl)

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






