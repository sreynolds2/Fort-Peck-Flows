# OBTAIN FUNCTIONS
source("./R/1_global-stochastic.r")
source("./R/2_functions-stochastic.r")
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
# inps<- project_environ(inputs = inputs,
#                        temp_data = temps,
#                        transitions = trans,
#                        temp_freq_data = temp_freq,
#                        years=500,
#                        reps = 8000,
#                        w_init = w,
#                        id="0_extend")

getinps<- readRDS("./output/_stochastic/Ntot_1-1.rds")
inps<- inputs
inps$environment<- getinps$inputs$environment
rm(getinps)

# SIMULATE INITIAL POPULATION
# inps<- init_pop(inputs, N0_type)
getinps<- readRDS("./output/_stochastic/Ntot_2020_PSPAP_1-1.rds")
inps$N0<- getinps$inputs$N0
inps$N0_type<- getinps$inputs$N0_type
rm(getinps)

## ENVIRONMENT 0 PATHS, 1000 YEARS AND 8000 REPS FOR
## AGE-0 SURVIVAL ANALYSIS
inps<- readRDS("inputs.rds")
# SIMULATE POPULATION REPS
## BASELINE & AGE-0 SURVIVAL GIVEN RETENTION
phi0MR<- c(0.000075, 0.95*0.000075, 1.05*0.000075,
           seq(0.0002, 0.002, 0.0002))
params<- data.frame(phi0_MR=phi0MR, param_id=1:length(phi0MR))

library(parallel)
numCores<- detectCores()
cl<- makeCluster(numCores)
# clusterEvalQ(cl, library(data.table))
clusterExport(cl, c("inps", "dat", "params", #init_pop,
                    "leslie", "project_pop"))
ptm<-proc.time()
phi0_test<- parLapply(cl, c(3,6,7,10,13),#1:nrow(params),
                      function(x)
{
  test<- project_pop(inputs = inps,
                     retention_data = dat,
                     gamma=0.5,
                     adjustments = list(phi0_MR=params$phi0_MR[x]),
                     years=1000,
                     reps=8000,
                     param_id=params$param_id[x])
  return(test)
})
tot<-(proc.time()-ptm)[3]/60
phi0_test$tot<- tot
tot
stopCluster(cl)
rm(cl)

# MAXIMUM AGE
# params<- rbind.fill(params, data.frame(max_age=c(99, 101),
#                                        param_id=1:2+max(params$param_id)))

# inps2<- inps
# inps2$max_age<- 99
# inps2$phi<- inps$phi[1:(length(inps$phi)-1)]
# inps2$psi<- inps$psi[1:(length(inps$psi)-1)]
# inps2$eggs<- inps$eggs[1:(length(inps$eggs)-1)]
# extra<- inps$N0[length(inps$N0)]
# v<- inps2$max_age-68+1
# ad<- rmultinom(1, extra, c(rep(0,67),rep(1/v,v)))
# inps2$N0<- inps$N0[1:(length(inps$N0)-1)]+ad
# rm(extra, v, ad)
# 
# test1<- project_pop(inputs = inps2,
#                     retention_data = dat,
#                     gamma=0.5,
#                     adjustments = list(),
#                     years=200,
#                     reps=5000,
#                     param_id=14)
# inps2$N0<- inps$N0[1:(length(inps$N0)-1)]
# test1<- project_pop(inputs = inps2,
#                     retention_data = dat,
#                     gamma=0.5,
#                     adjustments = list(),
#                     years=200,
#                     reps=5000,
#                     param_id="14_alt")
# 
# inps2<- inps
# inps2$max_age<- 101
# inps2$phi<- c(inps$phi, inps$phi[length(inps$phi)])
# inps2$psi<- c(inps$psi, inps$psi[length(inps$psi)])
# ## RUN EGG SIMULATION FOR 101 AND PULL NEW DATA
# e101<- read.csv("./baseline-parameters/fecundity_estimates_age_101.csv")
# e101<- e101$Mean_Eggs_Produced
# inps2$eggs<- c(inps$eggs, e101)
# inps2$N0<- c(inps$N0,0)
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
# # clusterEvalQ(cl, library(data.table))
# clusterExport(cl, c("inps", "dat", #"init_pop", 
#                     "leslie", "project_pop"))
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

# psi_params<- params[which(!is.na(params$psi)), c("psi", "age_id", "param_id")]
# library(parallel)
# numCores<- detectCores()
# cl<- makeCluster(numCores)
# # clusterEvalQ(cl, library(data.table))
# clusterExport(cl, c("inps", "dat", "psi_params", #"init_pop",
#                     "leslie", "project_pop"))
# invisible(parLapply(cl, 1:nrow(psi_params),
#                     function(x)
# {
#     inps2<- inps
#     inps2$psi[psi_params$age_id[x]]<- psi_params$psi[x]
#     test<- project_pop(inputs = inps2,
#                        retention_data = dat,
#                        gamma=0.5,
#                        adjustments = list(),
#                        years=200,
#                        reps=5000,
#                        param_id=psi_params$param_id[x])
# }))
# stopCluster(cl)
# rm(cl, psi_params)

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

# e_params<- params[which(!is.na(params$eggs)), c("eggs", "age_id", "param_id")]
# library(parallel)
# numCores<- detectCores()
# cl<- makeCluster(numCores)
# # clusterEvalQ(cl, library(data.table))
# clusterExport(cl, c("inps", "dat", "e_params", #"init_pop", 
#                     "leslie", "project_pop"))
# # ptm<-proc.time()
# eggs<- parLapply(cl, 1:nrow(e_params), 
#                  function(x)
# {
#   inps2<- inps
#   inps2$eggs[e_params$age_id[x]]<- e_params$eggs[x]
#   test<- project_pop(inputs = inps2,
#                       retention_data = dat,
#                       gamma=0.5,
#                       adjustments = list(),
#                       years=200,
#                       reps=5000,
#                       param_id=e_params$param_id[x])
#   return(test)
# })
# # tot<-(proc.time()-ptm)[3]/60
# # tot
# stopCluster(cl)
# rm(cl, e_params)

# SEX RATIO
# r<- data.frame(probF=c(0.95*inps$probF, 1.05*inps$probF),
#                param_id=1:2+max(params$param_id))
# params<- rbind.fill(params, r)

# params<- read.csv("./output/_stochastic/sens_elas_vals.csv")
# r<- params[!is.na(params$probF),]
# 
# library(parallel)
# cl<- makeCluster(2)
# # clusterEvalQ(cl, library(data.table))
# clusterExport(cl, c("inps", "dat", "r", #"init_pop", 
#                     "leslie", "project_pop"))
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
# g<- params[!is.na(params$gamma),]
# 
# library(parallel)
# cl<- makeCluster(2)
# # clusterEvalQ(cl, library(data.table))
# clusterExport(cl, c("inps", "dat", "g", #"init_pop", 
#                     "leslie", "project_pop"))
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
# ret<- params[!is.na(params$retention),]
# 
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
# write.csv(params, "./output/_stochastic/sens_elas_vals.csv",
#           row.names = FALSE)
params<- read.csv("./output/_stochastic/sens_elas_vals.csv")

## EXTINCTION ANALYSES
# library(parallel)
# numCores<- detectCores()
# cl<- makeCluster(numCores)
# clusterExport(cl, c("pseudo_extinct"))
# ptm<-proc.time()
# invisible(parLapply(cl, 1:567, function(p)
# {
#   dat<- readRDS(paste0("./output/_stochastic/Ntot_2020_PSPAP_1-", p, ".rds"))
#   out<-pseudo_extinct(pop_data = dat, threshold = 50,
#                       years=200, reps=5000)
#   saveRDS(out, paste0("./output/_stochastic/Full_Data_2020_PSPAP_1-", p, ".rds"))
# }))
# tot<-(proc.time()-ptm)[3]/60
# tot
# stopCluster(cl)
# rm(cl)
# 
# datt<- readRDS("./output/_stochastic/Ntot_2020_PSPAP_1-15_alt.rds")
# out<-pseudo_extinct(pop_data = datt, threshold = 50,
#                     years=200, reps=5000)
# saveRDS(out, "./output/_stochastic/Full_Data_2020_PSPAP_1-15_alt.rds")

### AGE-0 SURVIVAL ANALYSIS
# library(parallel)
# numCores<- detectCores()
# cl<- makeCluster(numCores)
# clusterExport(cl, c("pseudo_extinct"))
# invisible(parLapply(cl, 1:13, function(p)
# {
#   dat<- readRDS(paste0("./output/_stochastic/Ntot_2020_PSPAP_0-", p, ".rds"))
#   out<-pseudo_extinct(pop_data = dat, threshold = 50,
#                       years=1000, reps=8000)
#   saveRDS(out, paste0("./output/_stochastic/Full_Data_2020_PSPAP_0-", p, ".rds"))
# }))
# stopCluster(cl)
# rm(cl)


# TIME TO EXTINCTION
# ## TEST
# ### UNIFORM
# dat<- readRDS("./output/_stochastic/Full_Data_0-1.rds")
# rep_test<- lapply(seq(1000, 8000, 1000), function(r)
# {
#   out<-time_extinct(ext_data = dat, threshold = 50,
#                     years=200, reps=r)
#   out$reps<- r
#   return(out)
# })
# rep_test<- do.call("rbind", rep_test)
# 
# rep_diffE<- reshape2::dcast(rep_test, flow_scenario~reps, value.var="E_time")
# tmp<-lapply(1:nrow(rep_diffE), function(i)
# {
#   abs(diff(unlist(rep_diffE[i,2:9])))
# })
# tmp<- as.data.frame(do.call("rbind", tmp))
# rep_diffE<- cbind(data.frame(flow_scenario=rep_diffE[,1]), tmp)
# 
# rep_diff80<- reshape2::dcast(rep_test, flow_scenario~reps, value.var="time_80")
# tmp<-lapply(1:nrow(rep_diff80), function(i)
# {
#   abs(diff(unlist(rep_diff80[i,2:9])))
# })
# tmp<- as.data.frame(do.call("rbind", tmp))
# rep_diff80<- cbind(data.frame(flow_scenario=rep_diff80[,1]), tmp)
# ### 2020_PSPAP
# dat<- readRDS("./output/_stochastic/Full_Data_2020_PSPAP_1-1.rds")
# rep_test<- lapply(seq(1000, 5000, 1000), function(r)
# {
#   out<-time_extinct(ext_data = dat, threshold = 50,
#                     years=200, reps=r)
#   out$reps<- r
#   return(out)
# })
# rep_test<- do.call("rbind", rep_test)
# 
# rep_diffE<- reshape2::dcast(rep_test, flow_scenario~reps, value.var="E_time")
# tmp<-lapply(1:nrow(rep_diffE), function(i)
# {
#   abs(diff(unlist(rep_diffE[i,2:6])))
# })
# tmp<- as.data.frame(do.call("rbind", tmp))
# rep_diffE<- cbind(data.frame(flow_scenario=rep_diffE[,1]), tmp)
# 
# rep_diff80<- reshape2::dcast(rep_test, flow_scenario~reps, value.var="time_80")
# tmp<-lapply(1:nrow(rep_diff80), function(i)
# {
#   abs(diff(unlist(rep_diff80[i,2:6])))
# })
# tmp<- as.data.frame(do.call("rbind", tmp))
# rep_diff80<- cbind(data.frame(flow_scenario=rep_diff80[,1]), tmp)
# 
# ## FULL RUN
# ### UNIFORM
# library(parallel)
# numCores<- detectCores()
# cl<- makeCluster(numCores)
# clusterExport(cl, c("time_extinct"))
# ptm<-proc.time()
# ext<- parLapply(cl, 1:567, function(p)
# {
#   dat<- readRDS(paste0("./output/_stochastic/Full_Data_1-", p, ".rds"))
#   out<-time_extinct(ext_data = dat, threshold = 50,
#                       years=200, reps=5000)
#   return(out)
# })
# ext<- do.call("rbind", ext)
# tot<-(proc.time()-ptm)[3]/60
# tot
# stopCluster(cl)
# rm(cl)
# write.csv(ext, "./output/_stochastic/extinction_time_data_Uniform.csv",
#           row.names = FALSE)
# 
# ### 2020_PSPAP
# library(parallel)
# numCores<- detectCores()
# cl<- makeCluster(numCores)
# clusterExport(cl, c("time_extinct"))
# ptm<-proc.time()
# ext<- parLapply(cl, 1:567, function(p)
# {
#   dat<- readRDS(paste0("./output/_stochastic/Full_Data_2020_PSPAP_1-", p, ".rds"))
#   out<-time_extinct(ext_data = dat, threshold = 50,
#                     years=200, reps=5000)
#   return(out)
# })
# ext<- do.call("rbind", ext)
# tot<-(proc.time()-ptm)[3]/60
# tot
# stopCluster(cl)
# rm(cl)
# dat14<- readRDS("./output/_stochastic/Full_Data_2020_PSPAP_1-14_alt.rds")
# tmp<-time_extinct(ext_data = dat14, threshold = 50,
#                   years=200, reps=5000)
# ext<- rbind(ext, tmp)
# dat15<- readRDS("./output/_stochastic/Full_Data_2020_PSPAP_1-15_alt.rds")
# tmp<-time_extinct(ext_data = dat15, threshold = 50,
#                   years=200, reps=5000)
# ext<- rbind(ext, tmp)
# write.csv(ext, "./output/_stochastic/extinction_time_data_2020_PSPAP.csv",
#           row.names = FALSE)

# ### 2020_PSPAP AGE-0 SURVIVAL ANALYSIS
library(parallel)
numCores<- detectCores()
cl<- makeCluster(numCores)
clusterExport(cl, c("time_extinct"))
ext<- parLapply(cl, 1:13, function(p)
{
  dat<- readRDS(paste0("./output/_stochastic/Full_Data_2020_PSPAP_0-", p, ".rds"))
  out<-time_extinct(ext_data = dat, threshold = 50,
                    years=1000, reps=8000)
  return(out)
})
stopCluster(cl)
rm(cl)
ext<- do.call("rbind", ext)
write.csv(ext, "./output/_stochastic/age-0_extinction_time_2020_PSPAP.csv",
          row.names = FALSE)
# 
# # BASE COMPARISON
# ## UNIFORM
# uni<- read.csv("./output/_stochastic/extinction_time_data_Uniform.csv",
#                stringsAsFactors = FALSE)
# tmp<- uni[uni$param_id==1,]
# barplot(tmp$E_time[c(7,2,1,3,5,4,6)],
#         names.arg = tmp$flow_scenario[c(7,2,1,3,5,4,6)],
#         xlab="Management Alternative",
#         ylab="Expected Time to Psuedoextinction (Years)")
# plot(1:7, tmp$E_time[c(7,2,1,3,5,4,6)],
#      xaxt="n", xlab="Management Alternative",
#      ylab="Expected Time to Psuedoextinction (Years)", pch=19)
# axis(1, 1:7, tmp$flow_scenario[c(7,2,1,3,5,4,6)])
# 
# ## 2020 PSPAP
# pap<- read.csv("./output/_stochastic/extinction_time_data_2020_PSPAP.csv",
#                stringsAsFactors = FALSE)
# tmp<- pap[pap$param_id==1,]
# barplot(tmp$E_time[c(7,2,1,3,5,4,6)],
#         names.arg = tmp$flow_scenario[c(7,2,1,3,5,4,6)],
#         xlab="Management Alternative",
#         ylab="Expected Time to Psuedoextinction (Years)")
# plot(1:7, tmp$E_time[c(7,2,1,3,5,4,6)],
#      xaxt="n", xlab="Management Alternative",
#      ylab="Expected Time to Psuedoextinction (Years)", pch=19)
# axis(1, 1:7, tmp$flow_scenario[c(7,2,1,3,5,4,6)])
# 
#
# SENSITIVITIES & ELASTICITIES
# params<- read.csv("./output/_stochastic/sens_elas_vals.csv")
# params[params$param_id==1,c("max_age", "probF", "gamma")]<- c(100, 0.5, 0.5)
# source("./R/0_default-parameters.r")
# alts<- unique(uni$flow_scenario)
# ## UNIFORM
# sens<- lapply(alts, function(y)
# {
#   E_base<- uni[which(uni$param_id==1 &
#                       uni$flow_scenario==y),]$E_time
#   p_sens<- lapply(2:567, function(p)
#   {
#     E_comp<- uni[which(uni$param_id==p &
#                         uni$flow_scenario==y),]$E_time
#     tmp<- params[which(params$param_id==p),]
#     findP<- setdiff(names(tmp)[which(!is.na(tmp))],
#                     c("param_id", "age_id"))
#     if(findP %in% c("phi0_MR", "max_age", "probF", "gamma"))
#     {
#       j<- setdiff(which(!is.na(tmp)),
#                   match(c("param_id", "age_id"), names(tmp)))
#       s<- (E_comp-E_base)/(tmp[,j]-params[which(params$param_id==1),j])
#       e<- s*params[which(params$param_id==1),j]/E_base
#     }
#     if(findP=="phi")
#     {
#       s<- (E_comp-E_base)/(tmp$phi-inputs$phi[tmp$age_id])
#       e<- s*inputs$phi[tmp$age_id]/E_base
#     }
#     if(findP=="psi")
#     {
#       s<- (E_comp-E_base)/(tmp$psi-inputs$psi[tmp$age_id])
#       e<- s*inputs$psi[tmp$age_id]/E_base
#     }
#     if(findP=="eggs")
#     {
#       s<- (E_comp-E_base)/(tmp$eggs-inputs$eggs[tmp$age_id])
#       e<- s*inputs$eggs[tmp$age_id]/E_base
#     }
#     if(findP=="retention")
#     {
#       s<- NA
#       e<- abs(E_comp-E_base)/(E_base*0.05)
#     }
#     return(data.frame(sens=s, elas=e, param_id=p))
#   })
#   p_sens<- do.call("rbind", p_sens)
#   p_sens$flow_scenario<- y
#   p_sens<- p_sens[order(abs(p_sens$sens), decreasing = TRUE),]
#   return(p_sens)
# })
# saveRDS(sens, "./output/_stochastic/sens_elas_output_list_uniform.rds")
# sens<- do.call("rbind", sens)
# write.csv(sens, "./output/_stochastic/sens_elas_uniform.csv",
#           row.names = FALSE)
#
# ## 2020_PSPAP
# sensP<- lapply(alts, function(y)
# {
#   E_base<- pap[which(pap$param_id==1 &
#                        pap$flow_scenario==y),]$E_time
#   p_sens<- lapply(2:567, function(p)
#   {
#     E_comp<- pap[which(pap$param_id==p &
#                          pap$flow_scenario==y),]$E_time
#     tmp<- params[which(params$param_id==p),]
#     findP<- setdiff(names(tmp)[which(!is.na(tmp))],
#                     c("param_id", "age_id"))
#     if(findP %in% c("phi0_MR", "max_age", "probF", "gamma"))
#     {
#       j<- setdiff(which(!is.na(tmp)),
#                   match(c("param_id", "age_id"), names(tmp)))
#       s<- (E_comp-E_base)/(tmp[,j]-params[which(params$param_id==1),j])
#       e<- s*params[which(params$param_id==1),j]/E_base
#     }
#     if(findP=="phi")
#     {
#       s<- (E_comp-E_base)/(tmp$phi-inputs$phi[tmp$age_id])
#       e<- s*inputs$phi[tmp$age_id]/E_base
#     }
#     if(findP=="psi")
#     {
#       s<- (E_comp-E_base)/(tmp$psi-inputs$psi[tmp$age_id])
#       e<- s*inputs$psi[tmp$age_id]/E_base
#     }
#     if(findP=="eggs")
#     {
#       s<- (E_comp-E_base)/(tmp$eggs-inputs$eggs[tmp$age_id])
#       e<- s*inputs$eggs[tmp$age_id]/E_base
#     }
#     if(findP=="retention")
#     {
#       s<- NA
#       e<- abs(E_comp-E_base)/(E_base*0.05)
#     }
#     return(data.frame(sens=s, elas=e, param_id=p))
#   })
#   p_sens<- do.call("rbind", p_sens)
#   p_sens$flow_scenario<- y
#   p_sens<- p_sens[order(abs(p_sens$sens), decreasing = TRUE),]
#   return(p_sens)
# })
# saveRDS(sensP, "./output/_stochastic/sens_elas_output_list_2020_pspap.rds")
# sensP<- do.call("rbind", sensP)
# write.csv(sensP, "./output/_stochastic/sens_elas_2020_pspap.csv",
#           row.names = FALSE)

# PULL TOP SENSITIVITIES FOR EACH ALTERNATIVE
# ## UNIFORM
# sensU<-readRDS("./output/_stochastic/sens_elas_output_list_uniform.rds")
# sens_full<- lapply(1:7, function(y)
# {
#   tmp<- sensU[[y]]
#   tmp<- subset(tmp, !(param_id %in% 4:13))
#   tmp$param<- NA 
#   tmp$age<- NA
#   for(i in 1:nrow(tmp))
#   {
#     id<- tmp$param_id[i]
#     tmp$param[i]<- setdiff(names(params)[which(!is.na(params[params$param_id==id,]))],
#                            c("param_id", "age_id"))
#     tmp$age[i]<- params[params$param_id==id,]$age_id
#   }
#   return(tmp[,c(1,3:6)])
# })
# 
# indx<- c(1,7:283)
# sens_avg<- lapply(1:7, function(y)
# {
#   tmp<- sens_full[[y]]
#   out<- lapply(indx, function(x)
#   {
#     tmp2<- tmp[which(tmp$param_id %in% (2*x):(2*x+1)),]
#     return(data.frame(flow_scenario=tmp2[1,]$flow_scenario,
#                       sens=mean(tmp2$sens),
#                       param=tmp2[1,]$param,
#                       age=tmp2[1,]$age))
#   })
#   out<- do.call("rbind", out)
#   out<- out[order(out$sens, decreasing = TRUE),]
#   return(out)
# })
# saveRDS(sens_full, "./output/_stochastic/sensitivities_uniform.rds")
# saveRDS(sens_avg, "./output/_stochastic/average_sensitivities_uniform.rds")
#
# ## 2020 PSPAP
# sensP<-readRDS("./output/_stochastic/sens_elas_output_list_2020_pspap.rds")
# sens_fullP<- lapply(1:7, function(y)
# {
#   tmp<- sensP[[y]]
#   tmp<- subset(tmp, !(param_id %in% 4:13))
#   tmp$param<- NA 
#   tmp$age<- NA
#   for(i in 1:nrow(tmp))
#   {
#     id<- tmp$param_id[i]
#     tmp$param[i]<- setdiff(names(params)[which(!is.na(params[params$param_id==id,]))],
#                            c("param_id", "age_id"))
#     tmp$age[i]<- params[params$param_id==id,]$age_id
#   }
#   return(tmp[,c(1,3:6)])
# })
# 
# indx<- c(1,7:283)
# sens_avgP<- lapply(1:7, function(y)
# {
#   tmp<- sens_fullP[[y]]
#   out<- lapply(indx, function(x)
#   {
#     tmp2<- tmp[which(tmp$param_id %in% (2*x):(2*x+1)),]
#     return(data.frame(flow_scenario=tmp2[1,]$flow_scenario,
#                       sens=mean(tmp2$sens),
#                       param=tmp2[1,]$param,
#                       age=tmp2[1,]$age))
#   })
#   out<- do.call("rbind", out)
#   out<- out[order(out$sens, decreasing = TRUE),]
#   return(out)
# })
# saveRDS(sens_fullP, "./output/_stochastic/sensitivities_2020_pspap.rds")
# saveRDS(sens_avgP, "./output/_stochastic/average_sensitivities_2020_pspap.rds")
# 
# sens_avgP<- readRDS("./output/_stochastic/average_sensitivities_2020_pspap.rds")
# par(mfrow=c(3,3),
#     oma=c(1,1,0,0),
#     mar=c(2,4,4,2))
# invisible(lapply(1:7, function(y)
# {
#   tmp<- sens_avgP[[y]]
#   nms<- ifelse(is.na(tmp$age), tmp$param, 
#                paste0(tmp$param, "-", tmp$age))
#   barplot(tmp$sens[1:5],names.arg = nms[1:5], las=1, horiz = TRUE)
#   legend("topright", paste("Alternative", tmp$flow_scenario[1]),
#          bty="n")
# }))
# mtext("Sensitivity", 1, outer=TRUE, padj=-1)

# ELASTICITIES
# ## UNIFORM
# elasU<-readRDS("./output/_stochastic/sens_elas_output_list_uniform.rds")
# elas_full<- lapply(1:7, function(y)
# {
#   tmp<- elasU[[y]]
#   tmp<- subset(tmp, !(param_id %in% 4:13))
#   tmp$param<- NA
#   tmp$age<- NA
#   for(i in 1:nrow(tmp))
#   {
#     id<- tmp$param_id[i]
#     tmp$param[i]<- setdiff(names(params)[which(!is.na(params[params$param_id==id,]))],
#                            c("param_id", "age_id"))
#     tmp$age[i]<- params[params$param_id==id,]$age_id
#   }
#   tmp<- tmp[order(abs(tmp$elas), decreasing = TRUE),]
#   return(tmp[,2:6])
# })
# indx<- c(1,7:283)
# elas_avg<- lapply(1:7, function(y)
# {
#   tmp<- elas_full[[y]]
#   out<- lapply(indx, function(x)
#   {
#     tmp2<- tmp[which(tmp$param_id %in% (2*x):(2*x+1)),]
#     return(data.frame(flow_scenario=tmp2[1,]$flow_scenario,
#                       elas=mean(tmp2$elas),
#                       param=tmp2[1,]$param,
#                       age=tmp2[1,]$age))
#   })
#   out<- do.call("rbind", out)
#   out<- out[order(out$elas, decreasing = TRUE),]
#   return(out)
# })
# saveRDS(elas_full, "./output/_stochastic/elasticities_uniform.rds")
# saveRDS(elas_avg, "./output/_stochastic/average_elasticities_uniform.rds")

# ## 2020 PSPAP
# elasP<-readRDS("./output/_stochastic/sens_elas_output_list_2020_pspap.rds")
# elas_fullP<- lapply(1:7, function(y)
# {
#   tmp<- elasP[[y]]
#   tmp<- subset(tmp, !(param_id %in% 4:13))
#   tmp$param<- NA
#   tmp$age<- NA
#   for(i in 1:nrow(tmp))
#   {
#     id<- tmp$param_id[i]
#     tmp$param[i]<- setdiff(names(params)[which(!is.na(params[params$param_id==id,]))],
#                            c("param_id", "age_id"))
#     tmp$age[i]<- params[params$param_id==id,]$age_id
#   }
#   tmp<- tmp[order(abs(tmp$elas), decreasing = TRUE),]
#   return(tmp[,2:6])
# })
# indx<- c(1,7:283)
# elas_avgP<- lapply(1:7, function(y)
# {
#   tmp<- elas_fullP[[y]]
#   out<- lapply(indx, function(x)
#   {
#     tmp2<- tmp[which(tmp$param_id %in% (2*x):(2*x+1)),]
#     return(data.frame(flow_scenario=tmp2[1,]$flow_scenario,
#                       elas=mean(tmp2$elas),
#                       param=tmp2[1,]$param,
#                       age=tmp2[1,]$age))
#   })
#   out<- do.call("rbind", out)
#   out<- out[order(out$elas, decreasing = TRUE),]
#   return(out)
# })
# saveRDS(elas_fullP, "./output/_stochastic/elasticities_2020_pspap.rds")
# saveRDS(elas_avgP, "./output/_stochastic/average_elasticities_2020_pspap.rds")

# RANK SENSITIVITIES
## UNIFORM
### PARAMS
ranks<- lapply(c(1:3,14:567), function(i)
{
  tmp<- uni[which(uni$param_id==i),]
  out<- tmp[order(tmp$E_time, decreasing=TRUE),]$flow_scenario
  return(out)
})
test<- sapply(1:7, function(y)
{
  all(sapply(ranks, "[[", y)==ranks[[1]][y])
})
all(test)
## RANK ORDER DOES NOT CHANGE WITH PARAM CHANGES
ranks[[1]]
### AGE-0 SURVIVAL

### ADULT SURVIVAL???/MAXIMUM AGE


## 2020_PSPAP
### PARAMS
ranksP<- lapply(c(1:3,14:567), function(i)
{
  tmp<- pap[which(pap$param_id==i),]
  out<- tmp[order(tmp$E_time, decreasing=TRUE),]$flow_scenario
  return(out)
})
testP<- sapply(1:7, function(y)
{
  all(sapply(ranksP, "[[", y)==ranksP[[1]][y])
})
all(testP)
## RANK ORDER DOES NOT CHANGE WITH PARAM CHANGES
ranksP[[1]]
all(ranksP[[1]]==ranks[[1]])
### AGE-0 SURVIVAL

### ADULT SURVIVAL???/MAXIMUM AGE


# FRACTION EXTINCT
# library(parallel)
# numCores<- detectCores()
# cl<- makeCluster(numCores)
# clusterExport(cl, c("frac_extinct"))
# ptm<-proc.time()
# explore_ext<- parLapply(cl, 1:567, function(p)
# {
#   dat<- readRDS(paste0("./output/_stochastic/Full_Data_1-", p, ".rds"))
#   yr_test<- lapply(seq(75, 200, 25), function(y)
#   {
#     rep_no_test<- lapply(seq(1000, 5000, 1000), function(r)
#     {
#       out<- frac_extinct(ext_data = dat, threshold = 50,
#                          years=y, reps=r)
#       out$reps<- r
#       return(out)
#     })
#     rep_no_test<- do.call("rbind", rep_no_test)
#     rep_no_test$years<- y
#     return(rep_no_test)
#   })
#   yr_test<- do.call("rbind", yr_test)
#   return(yr_test)
# })
# ext_test<- do.call("rbind", explore_ext)
# tot<-(proc.time()-ptm)[3]/60
# tot
# stopCluster(cl)
# rm(cl)
# ext_test<- ext_test[order(ext_test$param_id,
#                           ext_test$years,
#                           ext_test$reps),]
# write.csv(ext_test, "./output/_stochastic/extinction_results.csv",
#           row.names = FALSE)

## COMPARE YEAR AND REP DATA
# ext<- read.csv("./output/_stochastic/extinction_results.csv",
#                stringsAsFactors = FALSE)
# bline<- ext[ext$param_id==1,]
# alts<- unique(bline$flow_scenario)
# yrs<- unique(bline$years)
# ### YEAR TEST
# #### ALL RUNS EXTINCT BY 100 YEARS FOR BASELINE AGE-0 SURVIVAL 
# tmp<- ext[ext$years>=100 & !(ext$param_id%in%2:13),]
# unique(tmp$frac_extinct)
# #### AFTER 75 YEARS VALUES HAVEN'T CONVERGED WITH 5000 REPS
# frac_diff<-sapply(alts, function(x)
# {
#   tmp<- bline[bline$years==75 & bline$flow_scenario==x,]
#   out<- min(abs(diff(tmp$frac_extinct)))
#   return(out)
# })
# max(frac_diff)
# #### RUN FINER YEAR MESH
# library(parallel)
# numCores<- detectCores()
# cl<- makeCluster(numCores)
# clusterExport(cl, c("frac_extinct"))
# explore_ext<- parLapply(cl, 1:567, function(p)
# {
#   dat<- readRDS(paste0("./output/_stochastic/Full_Data_1-", p, ".rds"))
#   yr_test<- lapply(seq(80, 95, 5), function(y)
#   {
#     rep_no_test<- lapply(seq(1000, 5000, 1000), function(r)
#     {
#       out<- frac_extinct(ext_data = dat, threshold = 50,
#                          years=y, reps=r)
#       out$reps<- r
#       return(out)
#     })
#     rep_no_test<- do.call("rbind", rep_no_test)
#     rep_no_test$years<- y
#     return(rep_no_test)
#   })
#   yr_test<- do.call("rbind", yr_test)
#   return(yr_test)
# })
# explore_ext<- do.call("rbind", explore_ext)
# stopCluster(cl)
# rm(cl)
# ext<- rbind(ext, explore_ext)
# ext<- ext[order(ext$param_id,
#                 ext$years,
#                 ext$reps),]
# write.csv(ext, "./output/_stochastic/extinction_results.csv",
#           row.names = FALSE)
# 
# #### RETEST
# bline<- ext[ext$param_id==1,]
# alts<- unique(bline$flow_scenario)
# yrs<- unique(bline$years)
# 
# frac_diff<-sapply(seq(75, 95, 5), function(y)
# {
#   outt<- sapply(alts, function(x)
#   {
#     tmp<- bline[bline$years==y & bline$flow_scenario==x,]
#     out<- min(abs(diff(tmp$frac_extinct)))
#     return(out)
#   })
#   return(max(outt))
# })
# frac_diff
# 
# frac_range<-sapply(seq(75, 95, 5), function(y)
# {
#   tmp<- bline[bline$years==y & bline$reps==5000,]
#   out<- max(tmp$frac_extinct)-min(tmp$frac_extinct)
#   return(out)
# })
# frac_range
# 
# ## EXTRA SIMULATIONS
# dat1<- readRDS("./output/_stochastic/Ntot_1-1.rds")
# dat2<- readRDS("./output/_stochastic/Ntot_2-1.rds")
# dat1$`1`<- cbind(dat1$`1`, dat2$`1`)
# dat1$`1a`<- cbind(dat1$`1a`, dat2$`1a`)
# dat1$`1b`<- cbind(dat1$`1b`, dat2$`1b`)
# dat1$`2`<- cbind(dat1$`2`, dat2$`2`)
# dat1$`2a`<- cbind(dat1$`2a`, dat2$`2a`)
# dat1$`2b`<- cbind(dat1$`2b`, dat2$`2b`)
# dat1$NoAct<- cbind(dat1$NoAct, dat2$NoAct)
# dat1$inputs$environment$environ_paths<- 
#   cbind(dat1$inputs$environment$environ_paths, dat2$inputs$environment$environ_paths)
# dat1$inputs$environment$notes<- "Environment paths 1 and 2 combined."
# dat1$inputs$reps<- dat1$inputs$reps + dat2$inputs$reps
# saveRDS(dat1, "./output/_stochastic/Ntot_0-1.rds")
# rm(dat2)
# out<-pseudo_extinct(pop_data = dat1, threshold = 50,
#                     years=200, reps=8000)
# saveRDS(out, "./output/_stochastic/Full_Data_0-1.rds")
# 
# dat<- out
# rm(dat1, out)
# yr_test<- lapply(c(seq(75, 100, 5), seq(125, 200, 25)), function(y)
# {
#   rep_no_test<- lapply(seq(1000, 8000, 1000), function(r)
#   {
#     out<- frac_extinct(ext_data = dat, threshold = 50,
#                        years=y, reps=r)
#     out$reps<- r
#     return(out)
#   })
#   rep_no_test<- do.call("rbind", rep_no_test)
#   rep_no_test$years<- y
#   return(rep_no_test)
# })
# yr_test<- do.call("rbind", yr_test)
# ext<- read.csv("./output/_stochastic/extinction_results.csv",
#                stringsAsFactors = FALSE)
# ext<- ext[which(ext$param_id!=1),]
# ext<- rbind(ext, yr_test)
# ext<- ext[order(ext$param_id,
#                 ext$years,
#                 ext$reps),]
# write.csv(ext, "./output/_stochastic/extinction_results.csv",
#           row.names = FALSE)
# 
# bline<- ext[ext$param_id==1,]
# alts<- unique(bline$flow_scenario)
# yrs<- unique(bline$years)
# 
# tmp<- bline[bline$years==yrs[1] & bline$flow_scenario==alts[1],]
# plot(tmp$reps, tmp$frac_extinct)


## COMPUTE SENSITIVITIES AND ELASTICITIES
## SPAWNING ANALYSIS


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