source("./R/1_global.r")
source("./R/2_functions.r")
full_run<- FALSE
source("./R/3_load-and-clean.r")
dat<- read.csv("./output/baseline_scenario_lambda_data.csv")

# NUMBERS ANALYSIS
## GENERATE STABLE_AGE POPULATIONS WITH VARIOUS NUMBERS OF ADULTS
adlts<- c(seq(100, 900, 100), seq(1000, 9000, 1000), 10000, 50000)
stable_age_pops<- lapply(1:length(adlts), function(x)
{
  tmp<- initialize_pop(inputs = inputs,
                       stable_age=TRUE,
                       initial_adults=adlts[x])
  tmp$type<- ifelse(tmp$stable_age, "stable_age", "boom_bust")
  tmp$id<- x
  saveRDS(tmp, paste0("./output/_populations/", tmp$type, "_pop_", tmp$id, ".rds"))
  return(tmp$N0)
})

## GENERATE AN INITAL POPULATION FROM BOOM AND BUST YEARS
### BASELINE VALUES
reps<- 1000
boom_bust_pops<- lapply(1:reps, function(x)
{
  tmp<- initialize_pop(inputs = inputs,
                       stable_age=FALSE,
                       boom_prob=1/5, 
                       bust_prob=1/5,
                       boom_recruits=25000,
                       bust_recruits=0,
                       avg_recruits=1000)
  tmp$type<- ifelse(tmp$stable_age, "stable_age", "boom_bust")
  tmp$id<- 1
  tmp$rep<- x
  saveRDS(tmp, paste0("./output/_populations/", tmp$type, "_pop_", tmp$id,
                      "-", tmp$rep, ".rds"))
  return(tmp$N0)
})
### RANDOMLY DRAWN VALUES
param_draws<- 100
pBoom<- runif(param_draws, 1/50, 1/2)
pBust<- runif(param_draws, 1/50, 1/2)
rBoom<- runif(param_draws, 1000, 50000)
rMax<- sapply(1:length(rBoom), function(x){min(1/2*rBoom[x], 5000)})
rAvg<- runif(param_draws, 100, rMax)
rBust<- runif(param_draws, 0, 10)
reps<- 100
invisible(lapply(1:param_draws, function(i)
{
  invisible(lapply(1:reps, function(x)
  {
    tmp<- initialize_pop(inputs = inputs,
                         stable_age=FALSE,
                         boom_prob=pBoom[i], 
                         bust_prob=pBust[i],
                         boom_recruits=rBoom[i],
                         bust_recruits=rBust[i],
                         avg_recruits=rAvg[i])
    tmp$type<- ifelse(tmp$stable_age, "stable_age", "boom_bust")
    tmp$id<- i+1
    tmp$rep<- x
    saveRDS(tmp, paste0("./output/_populations/", tmp$type, "_pop_", tmp$id,
                        "-", tmp$rep, ".rds"))
  }))
}))


## PROJECT THE POPULATIONS FORWARD UNDER EACH ALTERNATIVE SCENARIO
### USING DATA FROM 1953 TO 2012 IN ORDER
alts<- unique(dat$scenario)
pop_files<- dir("./output/_populations/")
scenario_ranks<- lapply(1:length(pop_files), function(y)
{
  pDat<- readRDS(paste0("./output/_populations/", pop_files[y]))
  id_rep<- ifelse(pDat$type=="boom_bust", paste0(pDat$id, "-", pDat$rep),
                  as.character(pDat$id))
  pop<- lapply(alts, function(x)
  {
    tmp<- subset(dat, scenario==x & temperature_flow=="M")
    tmp<- tmp[!duplicated(tmp$year),]
    tmp$year<- tmp$year-1953
    tmp<- subset(tmp, year>0)
    nyrs<- 2012-1953
    spnYr<- ifelse(1:nyrs %in% tmp$year, 1, 0)
    p_retained<- rep(0.001, nyrs)
    p_retained[tmp$year]<- tmp$p_retained
    pp<- project_pop(inputs=inputs,
                      init_inputs=pDat,
                      nyears=nyrs,
                      spnYr=spnYr,
                      p_retained=p_retained,
                      stocking=NULL)
    saveRDS(pp, paste0("./output/_projections/1935_to_2012_", x, "_", pDat$type,
                        "_", id_rep, ".rds"))
    out<- data.frame(Scenario=x, 
                     StartYear=1935, 
                     N0_type=pDat$type,
                     N0_id=id_rep,
                     N0_total=sum(pp$inputs$N0),
                     N0_adults=sum(pp$inputs$N0[15:60]),
                     pop_2012_total=sum(pp$pop_numbers[nrow(pp$pop_numbers),]),
                     pop_2012_adults=sum(pp$pop_numbers[nrow(pp$pop_numbers),15:60]))
    return(out)  
    })
    pop<- do.call(rbind, pop)
    pop$rank<- 0
    pop$rank[order(pop$pop_2012_total, decreasing = TRUE)]<- 1:length(alts)
    write.csv(pop, 
              paste0("./output/_ranks/1935_to_2012_Ranks_", pDat$type,
                        "_", id_rep, ".csv"), row.names = FALSE)
    return(pop)
})
ranks<- do.call(rbind, scenario_ranks)
write.csv(ranks, "./output/_ranks/1935_to_2012_Ranks_All.csv", row.names=FALSE)

test<- ddply(ranks, .(Scenario), summarize,
             min_rank=min(rank),
             max_rank=max(rank),
             mean_rank=mean(rank),
             median_rank=median(rank))
write.csv(test, "./output/_ranks/1935_to_2012_Ranks_Summary.csv", row.names=FALSE)


### USING DATA FROM 1930 TO 2012 PROBABILISTICALLY DRAWN
### FOR 10, 20, 50, AND 100 YEARS
alts<- unique(dat$scenario)
pop_files<- dir("./output/_populations/")
yrs<- c(10, 20, 50, 100)
reps<- 100

library(parallel)
## USE ALL CORES
numCores<-detectCores()
## INITIATE CLUSTER
cl<-makeCluster(numCores)
## MAKE PREVIOUS ITEMS AND FUNCTIONS AVAILABLE
clusterExport(cl, c("dat", "inputs", "alts", "pop_files", "yrs", "reps"),envir=environment())
clusterEvalQ(cl, source("./R/1_global.r"))
clusterEvalQ(cl, source("./R/2_functions.r"))
scenario_ranks<- parLapply(cl, 1026:length(pop_files), function(y)
{
  pDat<- readRDS(paste0("./output/_populations/", pop_files[y]))
  id_rep<- ifelse(pDat$type=="boom_bust", paste0(pDat$id, "-", pDat$rep),
                  as.character(pDat$id))
  pop<- lapply(alts, function(x)
  {
    tmp<- subset(dat, scenario==x & temperature_flow=="M")
    tmp<- tmp[!duplicated(tmp$year),]
    pop_reps<- lapply(1:reps, function(i)
    {
      p_retained<- rmultinom(max(yrs), 1, 
                             c(rep(1/(2012-1930+1), nrow(tmp)), 
                               1-nrow(tmp)/(2012-1930+1)))
      indx<- sapply(1:max(yrs), function(i){which(p_retained[,i]==1)})
      tmp<- rbind.fill(tmp, 
                       data.frame(scenario="NoRun",
                                  p_retained=0))
      p_retained<- tmp$p_retained[indx]
      spnYr<- ifelse(p_retained==0, 0, 1)
      pp<- project_pop(inputs=inputs,
                       init_inputs=pDat,
                       nyears=max(yrs),
                       spnYr=spnYr,
                       p_retained=p_retained,
                       stocking=NULL)
      N_tot<- rowSums(pp$pop_numbers)
      lambdas<- sapply(1:max(yrs), function(yr)
      {
        N_tot[yr+1]/N_tot[yr]
      })
      avg_lambda<- sapply(1:max(yrs), function(yr)
      {
        (N_tot[yr+1]/N_tot[1])^(1/yr)
      })
      pp$lambdas<- data.frame(AlternativeRan=spnYr,
                              year=1:max(yrs),
                              annual_GR=lambdas, 
                              avg_annual_GR=avg_lambda)
      saveRDS(pp, paste0("./output/_projections/Probabilistic_", x, "_", pDat$type,
                         "_", id_rep,"-", i, ".rds"))
      yrs<-c(0, yrs)
      out<- data.frame(Scenario=rep(x,length(yrs)), 
                       N0_type=rep(pDat$type,length(yrs)),
                       N0_id=rep(id_rep,length(yrs)),
                       Replicate=rep(i, length(yrs)),
                       Year=yrs,
                       N_total=N_tot[yrs+1],
                       N_adults=rowSums(pp$pop_numbers[yrs+1, 15:60]),
                       Avg_lambda=c(NA, pp$lambda$avg_annual_GR[yrs]))
      return(out)  
    })
    pop_reps<- do.call("rbind", pop_reps)
    saveRDS(pop_reps, paste0("./output/_projections/Probabilistic_", x, "_", pDat$type,
                             "_", id_rep,"_Summary_All_Reps.rds"))
    return(pop_reps)
  })
  pop<- do.call("rbind", pop)
  smry<- ddply(pop, .(Scenario, Year), summarize,
               N0_type=unique(N0_type),
               N0_id=unique(N0_id),
               N_min=min(N_total),
               N_5=quantile(N_total, 0.05),
               N_median=median(N_total),
               N_95=quantile(N_total, 0.95),
               N_max=max(N_total),
               N_mean=mean(N_total),
               avg_lambda_min=min(Avg_lambda),
               avg_lambda_5=quantile(Avg_lambda, 0.05, na.rm=TRUE),
               avg_lambda_median=median(Avg_lambda),
               avg_lambda_95=quantile(Avg_lambda, 0.95, na.rm=TRUE),
               avg_lambda_max=max(Avg_lambda),
               avg_lambda_mean=mean(Avg_lambda),
               geometric_lambda=prod(Avg_lambda)^(1/length(Avg_lambda)))
  smry<- smry[which(smry$Year %in% yrs),]
  smry$rank<- 0
  for(j in yrs)
  {
    indx<- which(smry$Year==j)
    rank<- order(smry$avg_lambda_median[indx], decreasing = TRUE)
    smry$rank[indx[rank]]<-1:length(alts)
  }
  write.csv(smry, 
            paste0("./output/_ranks/Probabilistic_Ranks_", pDat$type,
                   "_", id_rep, "_Summary_Across_Replicates.csv"), row.names = FALSE)
  test<- ddply(smry, .(Scenario), summarize,
               N0_type=unique(N0_type),
               N0_id=unique(N0_id),
               min_rank=min(rank),
               max_rank=max(rank),
               mean_rank=mean(rank),
               median_rank=median(rank))
  write.csv(test, paste0("./output/_ranks/Probabilistic_Ranks_", pDat$type,
                         "_", id_rep, "_Summary_Across_Replicates_and_Years.csv"), 
            row.names = FALSE)
  
  return(smry[,c(1:4,13,18)])
  #return(pop)
})
stopCluster(cl)
ranks<- do.call(rbind, scenario_ranks)
write.csv(ranks, "./output/_ranks/Probabilistic_Ranks_All_Populations.csv", 
          row.names=FALSE)

test<- ddply(ranks, .(Scenario, Year), summarize,
             min_rank=min(rank),
             max_rank=max(rank),
             mean_rank=mean(rank),
             median_rank=median(rank))
write.csv(test, 
          "./output/_ranks/Probabilistic_Ranks_Summary_Across_Populations.csv", 
          row.names=FALSE) 

test2<- ddply(ranks, .(Scenario), summarize,
              min_rank=min(rank),
              max_rank=max(rank),
              mean_rank=mean(rank),
              median_rank=median(rank))
write.csv(test2, 
          "./output/_ranks/Probabilistic_Ranks_Summary_Across_Populations_and_Years.csv", 
          row.names=FALSE)

### DO A MEDIAN ANALYSIS ACROSS COMPILED POPS
alts<- unique(dat$scenario)
pop_files<- dir("./output/_populations/")
yrs<- c(10, 20, 50, 100)
pop_reps<- lapply(c("boom_bust", "stable_age"), function(t)
{
  ids<- sapply(strsplit(pop_files, paste0(t, "_pop_")), "[[", 1)
  pop_rep_ids<- lapply(ids, function(i)
  {
    pop_alt<- lapply(alts, function(x)
    {
      yr_reps<- readRDS(paste0("./output/_projections/Probabilistic_", x, "_", t,
                                "_", i,"Summary_All_Reps.rds"))
      return(yr_reps)
    })
    pop_alt<- do.call("rbind", pop_alt)
    return(pop_alt)
  })
  pop_rep_ids<- do.call("rbind", pop_rep_ids)
  return(pop_rep_ids)
})
pop_reps<- do.call("rbind", pop_reps)
saveRDS(pop_reps, "./output/_projections/Probabilistic_Summary_All.rds")
smry<- ddply(pop_reps, .(Scenario, Year), summarize,
             N0_type=unique(N0_type),
             N0_id=unique(N0_id),
             N_min=min(N_total),
             N_5=quantile(N_total, 0.05),
             N_median=median(N_total),
             N_95=quantile(N_total, 0.95),
             N_max=max(N_total),
             N_mean=mean(N_total),
             avg_lambda_min=min(Avg_lambda),
             avg_lambda_5=quantile(Avg_lambda, 0.05, na.rm=TRUE),
             avg_lambda_median=median(Avg_lambda),
             avg_lambda_95=quantile(Avg_lambda, 0.95, na.rm=TRUE),
             avg_lambda_max=max(Avg_lambda),
             avg_lambda_mean=mean(Avg_lambda),
             geometric_lambda=prod(Avg_lambda)^(1/length(Avg_lambda)))
smry<- smry[which(smry$Year %in% yrs),]
smry$rank<- 0
for(j in yrs)
{
  indx<- which(smry$Year==j)
  rank<- order(smry$avg_lambda_median[indx], decreasing = TRUE)
  smry$rank[indx[rank]]<-1:length(alts)
}
write.csv(smry, 
          "./output/_ranks/Probabilistic_Ranks_Summary_Across_All_Population_Replicates.csv",
          row.names = FALSE)
test<- ddply(smry, .(Scenario), summarize,
             N0_type=unique(N0_type),
             N0_id=unique(N0_id),
             min_rank=min(rank),
             max_rank=max(rank),
             mean_rank=mean(rank),
             median_rank=median(rank))
write.csv(test, 
          "./output/_ranks/Probabilistic_Ranks_Summary_Across_All_Population_Replicates_and_Years.csv", 
          row.names = FALSE)


                         

    
## DO A YEAR 5 EXAMPLE TO SHOW NOT ENOUGH DIFFERENCEAND VERY STOCHASTIC

### USING DATA FROM 1962 TO 2012 PROBABILISTICALLY DRAWN
### FOR 10, 20, 50, AND 100 YEARS


### USING DATA FROM 1987 TO 2012 PROBABILISTICALLY DRAWN
### FOR 10, 20, 50, AND 100 YEARS

### USING DATA FROM 2000 TO 2012 PROBABILISTICALLY DRAWN
### FOR 10, 20, 50, AND 100 YEARS




##############################################################
##############################################################
##############################################################

# ORIGNAL RUNS WITH OLD FUNCTIONS THAT INITIALIZED THE 
# POPULATION WITHIN PROJECT_POP

## PROJECTION FROM 1953 (CLOSING OF GARRISON DAM) TO 2012
### ASSUMING 5000 ADULTS AND A STABLE AGE DISTRIBUTION IN 1953
alts<- unique(dat$scenario)
pop<- lapply(alts, function(x)
{
  tmp<- subset(dat, scenario==x & temperature_flow=="M")
  tmp<- tmp[!duplicated(tmp$year),]
  tmp$year<- tmp$year-1953
  tmp<- subset(tmp, year>0)
  nyears<- 2012-1953
  spnYr<- ifelse(1:nyears %in% tmp$year, 1, 0)
  p_retained<- rep(0.001, nyears)
  p_retained[tmp$year]<- tmp$p_retained
  #inps<-inputs
  #inps$phi[12:14]<-c(0.925, 0.935, 0.945) 
  #inps$phi[15:59]<- 0.95
  out<- project_pop(inputs=inputs,
                    nyears=nyears,
                    spnYr=spnYr,
                    p_retained=p_retained,
                    initial_adults = 7500)
  #initial_dist = TRUE,
  #N0=c(24369, rep(0, 59)))
  return(out)
})
saveRDS(pop, "./output/_projections/1935_to_2012_Projections_7500_34447.rds")
#saveRDS(pop, "./output/_projections/1935_to_2012_Projections_BackComputed_24369.rds")

final_pop<-lapply(1:length(alts), function(x)
{
  pop_trnd<- rowSums(pop[[x]]$pop_numbers)
  out<- pop_trnd[length(pop_trnd)]
  return(data.frame(scenario=alts[x], pop_2012=out))
})
final_pop<- do.call(rbind, final_pop)
final_pop$rank<- 0
final_pop$rank[order(final_pop$pop_2012, decreasing = TRUE)]<- 1:7
write.csv(final_pop, "./output/_ranks/1935_to_2012_Ranks_7500_34447.csv", row.names = FALSE)
#write.csv(final_pop, "./output/_ranks/1935_to_2012_Ranks_BackComputed_24369.csv", row.names = FALSE)




