# GENERATE INITIAL POPULATION
init_pop<- function(inputs=NULL,
                    type=NULL) # "Uniform", "2020_Rotella" -- NOT READY 
  # OR "2020_PSPAP"
{
  inps<- inputs
  ## 2020 AMCR MEDIAN POP EST
  inps$N_H<- 11853
  inps$N_W<- 505
  inps$sexratio_H<- 0.5
  inps$sexratio_W<- 0.32
  # SOURCE: THE 40:85 ADULT F:M RATIO ESTIMATED BY JAEGER ET AL. (2009) 
  # IN THE 2017 AM REPORT (TABLE 3-1)
  inps$N0_type<- type
  if(type=="Uniform")
  {
    N<- ceiling(inps$N_H*inps$sexratio_H+inps$N_W*inps$sexratio_W)
    N0<- rep(floor(N/inps$max_age), inps$max_age)
    N0<- N0+c(rep(1, N-sum(N0)), rep(0, inps$max_age-N+sum(N0)))
    inps$N0<- N0
  }
  if(type=="2020_PSPAP")
  {
    f_dat<- fread("../PSPAP-data/fish-processed.csv")
    names(f_dat)<-tolower(names(f_dat))
    f_dat[,rpma:=4]
    # set rpma for segments 5 and 6
    setDT(f_dat)[segment_id%in%c(5,6,23),rpma:=3]
    # set rpma for segment 1-4
    setDT(f_dat)[segment_id%in%c(1,2,3,4,21,22,51,52,53,54,55),rpma:=2]# 21 is Milk, 22 is YSR
    # SUBSET OUT UPPER RIVER FISH IN MOST RECENT YEARS
    f_dat<- subset(f_dat, rpma==2 & year>=2018)
    # READ IN STOCKING DATA
    stocked<- fread("../PSPAP-data/stocking-processed.csv")
    # REMOVE STOCKINGS WITH MISSING SPAWN DATE
    stocked[,spawn_date:= spawn.date]
    stocked<- subset(stocked, !is.na(spawn_date))
    # PULL RELEVANT COLUMNTS
    indx<- match(c("tag_number","spawn_date"), names(stocked))
    spawn<- stocked[,..indx]
    # REMOVE STOCKINGS WITH MISSING TAG NUMBERS
    tags<- unique(spawn$tag_number)
    tags<- tags[order(tags)]
    setDT(spawn)[tag_number %in% c("",tags[length(tags)], "..........","...",".",
                                   "0000000000","N/A","NOFISHSCAN","unknown",
                                   "XXXXXXXXXX","CAUDAL CL","N PIT"),
                 tag_number:="-99"]
    setDT(spawn)[is.na(tag_number),tag_number:="-99"]    
    setDT(spawn)[tag_number=="NA",tag_number:="-99"] 
    spawn<- subset(spawn, tag_number!="-99")
    # REMOVE DUPLICATE DATA
    spawn<- spawn[!duplicated(spawn),]
    # ADD SPAWNING DATE TO FISH DATA
    f_dat<- merge(f_dat, spawn, all.x=TRUE)
    # SUBSET OUT UNKNOWN AGE FISH
    f_dat<- subset(f_dat, !is.na(spawn_date))
    # KEEP MOST RECENT CAPTURE OF KNOWN AGE FISH
    tags<- f_dat[which(duplicated(f_dat$tag_number)),]$tag_number
    f_ids<- f_dat[which(f_dat$tag_number %in% tags),]$f_id
    indx<- sapply(tags, function(x)
    {
      tmp<- f_dat[which(f_dat$tag_number==x),]
      return(tmp[which.max(tmp$setdate),]$f_id)
      
    })
    f_ids<- setdiff(f_ids, indx)
    f_dat<- f_dat[-which(f_dat$f_id %in% f_ids),]
    rm(f_ids, tags, indx)
    # ADD IN AGE AT CAPTURE
    f_dat$age_capture<- floor(as.numeric(difftime(f_dat$setdate, 
                                                  f_dat$spawn_date,
                                                  units="days")/365.25))
    #f_dat$age<- as.numeric(difftime(f_dat$setdate, 
    #                                    dat$spawn.date,
    #                                    units="days")/365.25)
    ## CORRECT TYPO (BY INSPECTION OF DATA) IN SPAWN DATE 
    #dat[which(dat$stock_age<0),]$spawn.date<- as.POSIXct("1992-06-21")
    
    # COMPUTE AGE AT END OF 2020
    f_dat$age_2020<- as.numeric(difftime(as.POSIXct("2020-12-31"), 
                                         f_dat$spawn_date,
                                         units="days")/365.25)
    # AGGREGATE TO PROPORTIONS
    f_dat$age<- floor(f_dat$age_2020)
    f_dat$yr_diff<- f_dat$age-f_dat$age_capture
    f_dat$freq<- sapply(1:nrow(f_dat), function(i)
    {
      ifelse(f_dat$yr_diff[i]==0, 1,
             prod(inps$phi[f_dat$age_capture[i]:
                             (f_dat$age_capture[i] + f_dat$yr_diff[i]-1)]))
    })
    est<- aggregate(freq~age, f_dat, sum)
    est$prop<- est$freq/sum(est$freq)
    est$N0<- round(inps$N_H*inps$sexratio_H*est$prop)
    est<- merge(est, data.frame(age=1:inps$max_age), all=TRUE)
    est<- est[order(est$age),]
    # NO STOCKING IN 2020; ALL 2018 STOCKING SPAWNED IN 2017
    # ALL 2019 STOCKINGS FROM 2019 SPAWNING WERE DRIFT STUDY FISH
    est$N0[1]<- round(sum(stocked[which(stocked$year.stocked==2019 &
                                          as.numeric(format(stocked$spawn_date, "%Y"))==2019),]$numbers.stocked)*inps$phi0_MR*1)
    est$N0[2]<- round(sum(stocked[which(stocked$year.stocked==2019 &
                                          as.numeric(format(stocked$spawn_date, "%Y"))==2018),]$numbers.stocked)*prod(inps$phi[1]))
    est[is.na(est)]<- 0
    v<- inps$max_age-68+1
    est[which(est$age %in% 68:inps$max_age),]$N0<- rmultinom(1, ceiling(inps$N_W*inps$sexratio_W), rep(1/v, v))
    inps$N0<- est$N0
  }
  # if(type=="2020_Rotella")
  # {
  #   # READ IN ROTELLA 2016 ESTIMATES BY AGE
  #   s_dat<- extract_tables("./baseline-parameters/Rotella_2017_Update.pdf", 
  #                          pages=c(92))
  #   MO_Fing_Monthly<- as.data.frame(MO_Fing_Monthly[[1]][4:nrow(MO_Fing_Monthly[[1]]),])
  #   
  #   # READ IN STOCKING DATA
  #   stocked<- fread("../PSPAP-data/stocking-processed.csv")
  #   # SUBSET TO POST-2016
  #   stocked<- subset(stocked, year.stocked>=2017)
  #   # REMOVE STOCKINGS WITH MISSING SPAWN DATE
  #   stocked[,spawn_date:= spawn.date]
  #   stocked<- subset(stocked, !is.na(spawn_date))
  #   # ADD IN STOCKING AGE
  #   stocked$age_stocked<- round(as.numeric(difftime(stocked$stock.date, 
  #                                                   stocked$spawn_date,
  #                                                   units="days")/365.25))
  #   # ADD IN 2020 AGE
  #   stocked$age_2020<- round(as.numeric(difftime(as.POSIXct("2020-09-30"), 
  #                                                stocked$spawn_date,
  #                                                units="days")/365.25))
  #   stocked$yr_diff<- stocked$age_2020-stocked$age_stocked
  #   phi<- c(inps$phi0_MR, inps$phi)
  #   stocked$N_2020<- sapply(1:nrow(stocked), function(i)
  #   {
  #     stocked$numbers.stocked[i]*
  #       prod(phi[(stocked$age_stocked[i]+1):(stocked$age_stocked[i]+stocked$yr_diff[i])])
  #   })
  #   est<- aggregate(N_2020~age_2020, stocked, sum)
  #   est$N0<- round(est$N_2020)
  # }
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
                           w_init=NULL,
                           id=1)
{
  ## ERROR HANDLING
  if(!is.null(w_init) & length(w_init)!=reps)
  {
    return(print("Length of w_init must be equal to the number of replicates (reps)."))
  }
  temps<- temp_data
  trans<- transitions
  temp_freq<- temp_freq_data
  ## SIMULATE TEMPERATURE DATA
  W<- matrix(0, nrow=years, ncol=reps)
  if(is.null(w_init))
  {
    W[1,]<- sample(c("Low", "Median", "High"), reps, replace=TRUE,
                   prob=temp_freq$Frequency/sum(temp_freq$Frequency))
  }
  if(!is.null(w_init))
  {
    W[1,]<- w_init
  }
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
  inps<- leslie(inputs = inputs, 
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
    # write.csv(Nt, paste0("./output/_stochastic/Alt_", y, "_Ntot_",
    #                      inps$environment$id, "-", inps$param_id, ".csv"), 
    #           row.names = FALSE)
    return(Nt)
  })
  names(out)<- alts
  out$inputs<- inps
  saveRDS(out, paste0("./output/_stochastic/Ntot_", 
                      inps$N0_type, "_", inps$environment$id, 
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

time_extinct<- function(ext_data=NULL,
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
  alt_out<- lapply(1:7, function(x)
  {
    dat<- ext_data$extinction[[x]]
    if(all(dat$extinction_yr[1:reps]<=years))
    {
      tmp<- data.frame(extinction_yr=dat$extinction_yr[1:reps], freq=1)
      tmp<- aggregate(freq~extinction_yr, tmp, sum)
      tmp$prob<- tmp$freq/sum(tmp$freq)
      exp_time<- sum(tmp$extinction_yr*tmp$prob)
    }
    if(!all(dat$extinction_yr[1:reps]<=years))
    {
      exp_time<- -99
    }
    if(reps==length(dat$extinction_yr))
    {
      time_eighty<- min(which(dat$frac_extinct>=0.8))
    }
    if(reps<length(dat$extinction_yr))
    {
      frac_extinct<- dat$frac_extinct*length(dat$extinction_yr)/reps
      time_eighty<- min(which(frac_extinct>=0.8))
    }
    out<- data.frame(flow_scenario=names(ext_data$extinction)[x],
                     E_time=exp_time,
                     time_80=time_eighty)
    return(out)
  })
  alt_out<- do.call("rbind", alt_out)
  alt_out$param_id<- ext_data$inputs$param_id
  return(alt_out)
}

