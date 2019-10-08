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
alts<- unique(dat$scenario)
pop_files<- dir("./output/_populations/")
scenario_ranks<- lapply(1:length(pop_files), function(y)
{
  pDat<- readRDS(paste0("./output/_populations/", pop_files[y]))
  id_rep<- ifelse(pDat$type=="boom_bust", paste0(pDat$id, "-", pDat$rep),
                  pDat$id)
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




