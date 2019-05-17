
if(!exists("fullrun")){fullrun<- FALSE}

if(!fullrun)
{
  dat<- readRDS("./dat/drift_data.rds")
}

if(fullrun)
{
  chan<-odbcConnectExcel2007("./dat/UMORdrift_forPopModel_v1.xlsx")
  saklevels<-sqlFetch(chan,"SakLevels")
  names(saklevels)[3]<-"Lake_Sak_Upper_RM"
  scen<-sqlFetch(chan,"0pt5mps")
  dat<-scen
  scen<-sqlFetch(chan,"0pt7mps")
  dat<-rbind(dat,scen)
  scen<-sqlFetch(chan,"0pt9mps")
  dat<-rbind(dat,scen)
  odbcClose(chan)
  rm(chan,scen)
  
  scenarios<- expand.grid(U_mps=unique(dat$U_mps),
                          temp_C=unique(dat$temp_C),
                          Lake_Sak_Upper_RM=unique(saklevels$Lake_Sak_Upper_RM))
  
  scenarios$p_retained<-sapply(1:nrow(scenarios), function(i)
  {
    tmp<-subset(dat,
                U_mps==scenarios$U_mps[i] & 
                  temp_C==scenarios$temp_C[i])
    interp<-approxfun(tmp$RM, tmp$Percentile, 
                      method="linear", yleft=1, yright=0)
    out<-interp(scenarios$Lake_Sak_Upper_RM[i])
    return(round(out,3))
  })
  scenarios$p_conserv<-sapply(1:nrow(scenarios), function(i)
  {
    tmp<-subset(dat, 
                U_mps==scenarios$U_mps[i] & 
                  temp_C==scenarios$temp_C[i] &
                  RM>=scenarios$Lake_Sak_Upper_RM[i])
    out<-ifelse(nrow(tmp)>0, tmp[which.min(tmp$RM),"Percentile"], 0)
    return(out)
  })
  
  scenarios<-merge(scenarios, saklevels, by="Lake_Sak_Upper_RM")
  scenarios$spawn_rkm<- 1761*1.609
  scenarios<-scenarios[,c(2,3,6,7,1,8,4,5)]
  rm(dat, saklevels)
  saveRDS(scenarios, "./dat/drift_data.rds")
  dat<-scenarios
  rm(scenarios)
}
