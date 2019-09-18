## SENSITIVITY

dat_list<- dir("./output/Regions_of_Hope", pattern=".rds")
dat_list<- dat_list[-grep("_Variety_", dat_list)]


top_sens<- lapply(dat_list, function(x)
{
  dat<- readRDS(paste0("./output/Regions_of_Hope/", x))
  phi0<- strsplit(x, "phi0_")[[1]][2]
  phi0<- as.numeric(strsplit(phi0, "_")[[1]][1])
  b_id<- dat$birth_rate/100
  sens<- unlist(dat$sensitivities)
  sens<- sens[order(sens, decreasing = TRUE)]
  out<- data.frame(parameter=names(sens[1]),
                   sensitivity=unname(sens[1]),
                   rank=1,
                   phi0=phi0,
                   birthR=b_id)
  return(out)
})
top_sens<- do.call(rbind, top_sens)
unique(top_sens$parameter)
max(top_sens$sensitivity)
min(top_sens$sensitivity)

top_sens2<- lapply(dat_list, function(x)
{
  dat<- readRDS(paste0("./output/Regions_of_Hope/", x))
  phi0<- strsplit(x, "phi0_")[[1]][2]
  phi0<- as.numeric(strsplit(phi0, "_")[[1]][1])
  b_id<- dat$birth_rate/100
  sens<- unlist(dat$sensitivities)
  sens<- c(sens, sum(dat$sensitivities$phi[2:59]))
  names(sens)[length(sens)]<- "Age-2+ Survival" 
  sens<- sens[order(sens, decreasing = TRUE)]
  out<- data.frame(parameter=names(sens[1]),
                   sensitivity=unname(sens[1]),
                   rank=1,
                   phi0=phi0,
                   birthR=b_id)
  return(out)
})
top_sens2<- do.call(rbind, top_sens2)
unique(top_sens2$parameter)
sens_sum2<-aggregate(rank~parameter, top_sens2, sum)
sens_sum2<- sens_sum2[order(sens_sum2$rank, decreasing = TRUE),]
sens_sum2$rank<- sens_sum2$rank/sum(sens_sum2$rank)*100
names(sens_sum2)[2]<-"Percentage"
max(top_sens2$sensitivity)
min(top_sens2$sensitivity)

top_elas<- lapply(dat_list, function(x)
{
  dat<- readRDS(paste0("./output/Regions_of_Hope/", x))
  phi0<- strsplit(x, "phi0_")[[1]][2]
  phi0<- as.numeric(strsplit(phi0, "_")[[1]][1])
  b_id<- dat$birth_rate/100
  elas<- unlist(dat$elasticities)
  elas<- elas[order(elas, decreasing = TRUE)]
  out<- data.frame(parameter=names(elas[1]),
                   elasticity=unname(elas[1]),
                   rank=1,
                   phi0=phi0,
                   birthR=b_id)
  return(out)
})
top_elas<- do.call(rbind, top_elas)
unique(top_elas$parameter)
summary<-aggregate(rank~parameter, top_elas, sum)
summary<- summary[order(summary$rank, decreasing = TRUE),]

meaningful<- c()
for(i in 1:59)
{
  meaningful<-c(meaningful, (i-1)*60+i+1)
}
#meaninful<- c(meaningful, seq(1, 60*59+1, 60))

ages<- sapply(summary$parameter[grep("entries", summary$parameter)],
              function(x)
{
  val<- as.numeric(strsplit(as.character(x), "entries")[[1]][2])
  out<- which(meaningful==val)
  return(out)
})
  

summary$parameter<- as.character(summary$parameter)
summary$parameter[grep("entries", summary$parameter)]<- 
  paste0("Age-", ages, " Survival")
summary$rank<- summary$rank/sum(summary$rank)*100
names(summary)[2]<- "Percent of Models"
summary$rank<- 1

write.csv(summary, "./output/Regions_of_Hope/elasticity_table.csv",
          row.names = FALSE)

max(top_elas$elasticity)
min(top_elas$elasticity)


top_elas2<- lapply(dat_list, function(x)
{
  dat<- readRDS(paste0("./output/Regions_of_Hope/", x))
  phi0<- strsplit(x, "phi0_")[[1]][2]
  phi0<- as.numeric(strsplit(phi0, "_")[[1]][1])
  b_id<- dat$birth_rate/100
  phi<- c()
  for(i in 3:nrow(dat$A))
  {
    phi<- c(phi, dat$A[i,i-1])
  }
  elas<- unlist(dat$elasticities)
  elas<- c(elas, sum(dat$sensitivities$phi[2:59]*phi/dat$lambda1))
  names(elas)[length(elas)]<- "Age-2+ Survival" 
  elas<- elas[order(elas, decreasing = TRUE)]
  out<- data.frame(parameter=names(elas[1]),
                   sensitivity=unname(elas[1]),
                   rank=1,
                   phi0=phi0,
                   birthR=b_id)
  return(out)
})
top_elas2<- do.call(rbind, top_elas2)
unique(top_elas2$parameter)



