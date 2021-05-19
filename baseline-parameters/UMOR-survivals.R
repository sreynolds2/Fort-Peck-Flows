### NEED TO DO YELLOWSTONE RIVER YEARLINGS


# install.packages("tabulizer")
library(tabulizer)
library(plyr)

setwd("C:/Users/sreynolds/Documents/GitHub/Fort-Peck-Flows")

###############################################
#                                             #
#         MISSOURI RIVER FINGERLINGS          #
#                                             #
###############################################
MO_Fing_Monthly<- extract_tables("./baseline-parameters/Rotella_2017_Update.pdf", 
                                pages=c(29))
MO_Fing_Monthly<- as.data.frame(MO_Fing_Monthly[[1]][4:nrow(MO_Fing_Monthly[[1]]),])

names(MO_Fing_Monthly)<- c("Start Date", "End Date", "Months", 
                           "Age at Interval Start (Months)", 
                           "Age at Interval End (Months)", 
                           "Monthly Survival", "Survival SE", 
                           "Proportion of Population Still Alive", 
                           "Proportion SE", 
                           "95% CI for Cumulative Proportion Surviving")



MO_Fing_Annual<- extract_tables("./baseline-parameters/Rotella_2017_Update.pdf", 
                              pages=c(30))


MO_Fing_Annual<- as.data.frame(MO_Fing_Annual[[1]][4:nrow(MO_Fing_Annual[[1]]), 
                                               2:ncol(MO_Fing_Annual[[1]])])
names(MO_Fing_Annual)<- c("Years Since Release", "Months Since Release", 
                "Interval Survival Rate", "SE", "Release Type", "RPMA")



MO_FNG<- extract_tables("./baseline-parameters/Rotella_2017_Update.pdf", 
                         pages=c(33, 34, 35))
MO_FNG<- lapply(1:length(MO_FNG), function(x)
{
  out<- as.data.frame(MO_FNG[[x]][4:nrow(MO_FNG[[x]]), 
                                  2:ncol(MO_FNG[[x]])])
  names(out)<- c("Cohort", "Type", "Start Date", "End Date", 
                 "Age at Interval Start (Months)", 
                 "Age at Interval End (Months)", 
                 "Proportion of Population Still Alive", 
                 "Proportion SE", "N at Interval End", 
                 "95% CI for N at Interval End")
  out$id<- 1:nrow(out)
  return(out)
})
MO_FNG<- do.call(rbind, MO_FNG)
MO_FNG$Cohort<- as.numeric(as.character(MO_FNG$Cohort))
MO_FNG<- subset(MO_FNG, Cohort %in% c(3,4,6,7))
MO_FNG$`Start Date`<- strptime(MO_FNG$`Start Date`, "%m/%d/%y")
MO_FNG$`End Date`<- strptime(MO_FNG$`End Date`, "%m/%d/%y")
MO_FNG$`N at Interval End`<- as.numeric(gsub(",", "", as.character(MO_FNG$`N at Interval End`)))
#MO_FNG$Months<- as.numeric(as.character(MO_FNG$`Age at Interval End (Months)`))-as.numeric(as.character(MO_FNG$`Age at Interval Start (Months)`)) 
MO_FNG$Days<- as.numeric(difftime(MO_FNG$`End Date`, MO_FNG$`Start Date`, 
                                        units="days"))
tmp<- data.frame(Cohort=c(3,4,6,7),
                 Release_Number=c(4656, 20264, 20207, 1998))
MO_FNG<- merge(MO_FNG, tmp, by="Cohort", all.x=TRUE)
rm(tmp)
MO_FNG$`N at Interval Start`<- NA
MO_FNG$`N at Interval Start`[2:nrow(MO_FNG)]<- MO_FNG$`N at Interval End`[1:(nrow(MO_FNG)-1)]
MO_FNG[which(MO_FNG$id==1),]$`N at Interval Start`<- MO_FNG[which(MO_FNG$id==1),]$Release_Number
MO_FNG$Interval_Proportion<- MO_FNG$`N at Interval End`/MO_FNG$`N at Interval Start`
#MO_FNG$`Monthly Survival`<- MO_FNG$Interval_Proportion^(1/MO_FNG$Months)
MO_FNG$`Daily Survival`<- MO_FNG$Interval_Proportion^(1/MO_FNG$Days)


phi_MO_fing<- lapply(unique(MO_FNG$Cohort), function(x)
{
  dat<- MO_FNG[which(MO_FNG$Cohort==x),]
  min_year<- as.numeric(format(min(dat$`Start Date`), "%Y"))
  max_year<- as.numeric(format(max(dat$`Start Date`), "%Y"))
  birthdays<- strptime(paste0(min_year:max_year, "-06-01"), "%Y-%m-%d")
  params<- paste0("phi", c("_fing", 1:(length(birthdays)-2)))
  vals<- sapply(2:length(birthdays), function(y)
  {
    tmp<-dat[which(dat$`Start Date`<=birthdays[y] & dat$`End Date`>birthdays[y-1]),]
    tmp$Days[nrow(tmp)]<- as.numeric(unname(difftime(birthdays[y], 
                                                     max(tmp$`Start Date`[nrow(tmp)],
                                                         birthdays[y-1]),
                                                     units = "days"))) 
    if(y>2 & nrow(tmp)>1)
    {
      tmp$Days[1]<- as.numeric(unname(difftime(tmp$`End Date`[1], birthdays[y-1], units = "days"))) 
    }
    surv<-prod(tmp$`Daily Survival`^tmp$Days)
    return(surv)
  })
  weights<- unique(dat$Release_Number)
  for(i in 1:(length(vals)-1))
  {
    weights<- c(weights, weights[i]*vals[i])
  }
  out<- data.frame(Cohort=x, 
                   Parameter=params,
                   Estimate=vals,
                   Numbers=weights)
  return(out)
})
phi_MO_fing<- do.call(rbind, phi_MO_fing)
phi_MO_fing$Weighted_Val<- phi_MO_fing$Estimate*phi_MO_fing$Numbers

# tmp<- ddply(phi_MO_fing, .(Parameter), summarize,
#                     unweighted_mean=mean(Estimate),
#                     weighted_mean=sum(Weighted_Val)/sum(Numbers))
# max(abs(tmp$unweighted_mean-tmp$weighted_mean))
# # 0.0008075766  
# ## VERY LITTLE DIFFERENCE BETWEEN WEIGHTED AND UNWEIGHTED MEANS

phi_MO_fing<- ddply(phi_MO_fing, .(Parameter), summarize,
                    Estimate=sum(Weighted_Val)/sum(Numbers))
phi_MO_fing$River<- "MO"
phi_MO_fing$Type<- "fingerling"

rm(MO_Fing_Annual, MO_Fing_Monthly, MO_FNG)
phi<- data.frame(Parameter=phi_MO_fing$Parameter,
                 MO_fing=phi_MO_fing$Estimate)

###############################################
#                                             #
#       YELLOWSTONE RIVER FINGERLINGS         #
#                                             #
###############################################
YE_Fing_Monthly<- extract_tables("./baseline-parameters/Rotella_2017_Update.pdf", 
                                 pages=c(37))
YE_Fing_Monthly<- as.data.frame(YE_Fing_Monthly[[1]][4:nrow(YE_Fing_Monthly[[1]]),])

names(YE_Fing_Monthly)<- c("Start Date", "End Date", "Months", 
                           "Age at Interval Start (Months)", 
                           "Age at Interval End (Months)", 
                           "Monthly Survival", "Survival SE", 
                           "Proportion of Population Still Alive", 
                           "Proportion SE", 
                           "95% CI for Cumulative Proportion Surviving")



YE_Fing_Annual<- extract_tables("./baseline-parameters/Rotella_2017_Update.pdf", 
                                pages=c(38))


YE_Fing_Annual<- as.data.frame(YE_Fing_Annual[[1]][4:nrow(YE_Fing_Annual[[1]]), 
                                                   2:ncol(YE_Fing_Annual[[1]])])
names(YE_Fing_Annual)<- c("Years Since Release", "Months Since Release", 
                          "Interval Survival Rate", "SE", "Release Type", "RPMA")



YE_FNG<- extract_tables("./baseline-parameters/Rotella_2017_Update.pdf", 
                        pages=c(41:44))
YE_FNG[[6]]<-NULL
YE_FNG<- lapply(1:length(YE_FNG), function(x)
{
  out<- as.data.frame(YE_FNG[[x]][4:nrow(YE_FNG[[x]]), 
                                  2:ncol(YE_FNG[[x]])])
  names(out)<- c("Cohort", "Type", "Start Date", "End Date", 
                 "Age at Interval Start (Months)", 
                 "Age at Interval End (Months)", 
                 "Proportion of Population Still Alive", 
                 "Proportion SE", "N at Interval End", 
                 "95% CI for N at Interval End")
  out$id<- 1:nrow(out)
  return(out)
})
YE_FNG<- do.call(rbind, YE_FNG)
YE_FNG$Cohort<- as.numeric(as.character(YE_FNG$Cohort))
YE_FNG<- subset(YE_FNG, Cohort %in% c(3,4,6,7))
YE_FNG$`Start Date`<- strptime(YE_FNG$`Start Date`, "%m/%d/%y")
YE_FNG$`End Date`<- strptime(YE_FNG$`End Date`, "%m/%d/%y")
YE_FNG$`N at Interval End`<- as.numeric(gsub(",", "", as.character(YE_FNG$`N at Interval End`)))
#YE_FNG$months<- as.numeric(as.character(YE_FNG$`Age at Interval End (Months)`))-as.numeric(as.character(YE_FNG$`Age at Interval Start (Months)`)) 
YE_FNG$Days<- as.numeric(difftime(YE_FNG$`End Date`, YE_FNG$`Start Date`, 
                                  units="days"))
tmp<- data.frame(Cohort=c(3,4,6,7),
                 Release_Number=c(2262, 21202, 20781, 2000))
YE_FNG<- merge(YE_FNG, tmp, by="Cohort", all.x=TRUE)
rm(tmp)
YE_FNG$`N at Interval Start`<- NA
YE_FNG$`N at Interval Start`[2:nrow(YE_FNG)]<- YE_FNG$`N at Interval End`[1:(nrow(YE_FNG)-1)]
YE_FNG[which(YE_FNG$id==1),]$`N at Interval Start`<- YE_FNG[which(YE_FNG$id==1),]$Release_Number
YE_FNG$Interval_Proportion<- YE_FNG$`N at Interval End`/YE_FNG$`N at Interval Start`
#YE_FNG$`Monthly Survival`<- YE_FNG$Interval_Proportion^(1/YE_FNG$Months)
YE_FNG$`Daily Survival`<- YE_FNG$Interval_Proportion^(1/YE_FNG$Days)


phi_YE_fing<- lapply(unique(YE_FNG$Cohort), function(x)
{
  dat<- YE_FNG[which(YE_FNG$Cohort==x),]
  min_year<- as.numeric(format(min(dat$`Start Date`), "%Y"))
  max_year<- as.numeric(format(max(dat$`Start Date`), "%Y"))
  birthdays<- strptime(paste0(min_year:max_year, "-06-01"), "%Y-%m-%d")
  params<- paste0("phi", c("_fing", 1:(length(birthdays)-2)))
  vals<- sapply(2:length(birthdays), function(y)
  {
    tmp<-dat[which(dat$`Start Date`<=birthdays[y] & dat$`End Date`>birthdays[y-1]),]
    tmp$Days[nrow(tmp)]<- as.numeric(unname(difftime(birthdays[y], 
                                                     max(tmp$`Start Date`[nrow(tmp)],
                                                         birthdays[y-1]),
                                                     units = "days"))) 
    if(y>2 & nrow(tmp)>1)
    {
      tmp$Days[1]<- as.numeric(unname(difftime(tmp$`End Date`[1], birthdays[y-1], units = "days"))) 
    }
    surv<-prod(tmp$`Daily Survival`^tmp$Days)
    return(surv)
  })
  weights<- unique(dat$Release_Number)
  for(i in 1:(length(vals)-1))
  {
    weights<- c(weights, weights[i]*vals[i])
  }
  out<- data.frame(Cohort=x, 
                   Parameter=params,
                   Estimate=vals,
                   Numbers=weights)
  return(out)
})
phi_YE_fing<- do.call(rbind, phi_YE_fing)
phi_YE_fing$Weighted_Val<- phi_YE_fing$Estimate*phi_YE_fing$Numbers

phi_YE_fing<- ddply(phi_YE_fing, .(Parameter), summarize,
                    Estimate=sum(Weighted_Val)/sum(Numbers))
phi_YE_fing$River<- "YE"
phi_YE_fing$Type<- "fingerling"

rm(YE_Fing_Annual, YE_Fing_Monthly, YE_FNG)
phi<- merge(phi, data.frame(Parameter=phi_YE_fing$Parameter,
                            YE_fing=phi_YE_fing$Estimate),
            by="Parameter", all=TRUE)



####################################################
#                                                  #
#         MISSOURI RIVER SPRING YEARLINGS          #
#                                                  #
####################################################
MO_SP_Monthly<- extract_tables("./baseline-parameters/Rotella_2017_Update.pdf", 
                                    pages=c(47))
MO_SP_Monthly<- as.data.frame(MO_SP_Monthly[[1]][4:nrow(MO_SP_Monthly[[1]]),])

names(MO_SP_Monthly)<- c("Start Date", "End Date", "Months", 
                          "Age at Interval Start (Months)", 
                          "Age at Interval End (Months)", 
                          "Monthly Survival", "Survival SE", 
                          "Proportion of Population Still Alive", 
                          "Proportion SE", 
                          "95% CI for Cumulative Proportion Surviving")



MO_SP_Annual<- extract_tables("./baseline-parameters/Rotella_2017_Update.pdf", 
                              pages=c(48))


MO_SP_Annual<- as.data.frame(MO_SP_Annual[[1]][4:nrow(MO_SP_Annual[[1]]), 
                                               2:ncol(MO_SP_Annual[[1]])])
names(MO_SP_Annual)<- c("Years Since Release", "Months Since Release", 
                          "Interval Survival Rate", "SE", "Release Type", "RPMA")



MO_SP<- extract_tables("./baseline-parameters/Rotella_2017_Update.pdf", 
                        pages=c(49, 50, 52, 54, 55))
MO_SP<- lapply(1:length(MO_SP), function(x)
{
  if(x<7)
  {
    out<- as.data.frame(MO_SP[[x]][4:nrow(MO_SP[[x]]), 
                                   2:ncol(MO_SP[[x]])])
  }
  if(x==7 | x==8)
  {
    out<- as.data.frame(MO_SP[[x]][4:nrow(MO_SP[[x]]),])
  }
  if(x==9)
  {
    out<- as.data.frame(matrix(MO_SP[[x]][4,], nrow=1))
  }
  names(out)<- c("Cohort", "Type", "Start Date", "End Date", 
                 "Age at Interval Start (Months)", 
                 "Age at Interval End (Months)", 
                 "Proportion of Population Still Alive", 
                 "Proportion SE", "N at Interval End", 
                 "95% CI for N at Interval End")
  out$id<- 1:nrow(out)
  return(out)
})
MO_SP<- do.call(rbind, MO_SP)
MO_SP$Cohort<- as.numeric(as.character(MO_SP$Cohort))
MO_SP<- subset(MO_SP, Cohort %in% c(1,2,4,7:12))
MO_SP$`Start Date`<- strptime(MO_SP$`Start Date`, "%m/%d/%y")
MO_SP$`End Date`<- strptime(MO_SP$`End Date`, "%m/%d/%y")
MO_SP$`N at Interval End`<- as.numeric(gsub(",", "", as.character(MO_SP$`N at Interval End`)))
#MO_SP$Months<- as.numeric(as.character(MO_SP$`Age at Interval End (Months)`))-as.numeric(as.character(MO_SP$`Age at Interval Start (Months)`)) 
MO_SP$Days<- as.numeric(difftime(MO_SP$`End Date`, MO_SP$`Start Date`, 
                                  units="days"))
tmp<- data.frame(Cohort=c(1,2,4,7:12),
                 Release_Number=c(821, 558, 1922, 5179, 1592, 375, 423, 
                                  314, 710))
MO_SP<- merge(MO_SP, tmp, by="Cohort", all.x=TRUE)
rm(tmp)
MO_SP$`N at Interval Start`<- NA
MO_SP$`N at Interval Start`[2:nrow(MO_SP)]<- MO_SP$`N at Interval End`[1:(nrow(MO_SP)-1)]
MO_SP[which(MO_SP$id==1),]$`N at Interval Start`<- MO_SP[which(MO_SP$id==1),]$Release_Number
MO_SP$Interval_Proportion<- MO_SP$`N at Interval End`/MO_SP$`N at Interval Start`
#MO_SP$`Monthly Survival`<- MO_SP$Interval_Proportion^(1/MO_SP$Months)
MO_SP$`Daily Survival`<- MO_SP$Interval_Proportion^(1/MO_SP$Days)


phi_MO_sp<- lapply(unique(MO_SP$Cohort), function(x)
{
  dat<- MO_SP[which(MO_SP$Cohort==x),]
  min_year<- as.numeric(format(min(dat$`Start Date`), "%Y"))-1
  max_year<- as.numeric(format(max(dat$`Start Date`), "%Y"))
  birthdays<- strptime(paste0(min_year:max_year, "-06-01"), "%Y-%m-%d")
  if(length(birthdays)>=3)
  {
    params<- paste0("phi", c("_sp", 1:(length(birthdays)-2)))
  }
  if(length(birthdays)==2)
  {
    params<-"phi_sp"
  }
  vals<- sapply(2:length(birthdays), function(y)
  {
    tmp<-dat[which(dat$`Start Date`<=birthdays[y] & dat$`End Date`>birthdays[y-1]),]
    tmp$Days[nrow(tmp)]<- as.numeric(unname(difftime(birthdays[y], 
                                                     max(tmp$`Start Date`[nrow(tmp)],
                                                         birthdays[y-1]),
                                                     units = "days"))) 
    if(y>2 & nrow(tmp)>1)
    {
      tmp$Days[1]<- as.numeric(unname(difftime(tmp$`End Date`[1], birthdays[y-1], units = "days"))) 
    }
    surv<-prod(tmp$`Daily Survival`^tmp$Days)
    return(surv)
  })
  weights<- unique(dat$Release_Number)
  if(length(birthdays)>2)
  {
    for(i in 1:(length(vals)-1))
    {
      weights<- c(weights, weights[i]*vals[i])
    }
  }
  out<- data.frame(Cohort=x, 
                   Parameter=params,
                   Estimate=vals,
                   Numbers=weights)
  return(out)
})
phi_MO_sp<- do.call(rbind, phi_MO_sp)
phi_MO_sp$Weighted_Val<- phi_MO_sp$Estimate*phi_MO_sp$Numbers

# plot(phi_MO_sp[which(phi_MO_sp$Parameter=="phi3"),]$Cohort, 
#      phi_MO_sp[which(phi_MO_sp$Parameter=="phi3"),]$Estimate, 
#      ylim=c(min(phi_MO_sp$Estimate, na.rm = TRUE), 
#             max(phi_MO_sp$Estimate, na.rm = TRUE)))
# points(phi_MO_sp[which(phi_MO_sp$Parameter=="phi4"),]$Cohort, 
#        phi_MO_sp[which(phi_MO_sp$Parameter=="phi4"),]$Estimate, 
#        col="red")
# points(phi_MO_sp[which(phi_MO_sp$Parameter=="phi2"),]$Cohort, 
#        phi_MO_sp[which(phi_MO_sp$Parameter=="phi2"),]$Estimate, 
#        col="blue")

phi_MO_sp<- ddply(phi_MO_sp, .(Parameter), summarize,
                    Estimate=sum(Weighted_Val)/sum(Numbers))
phi_MO_sp$River<- "MO"
phi_MO_sp$Type<- "spring yearling"
phi_MO_sp<- phi_MO_sp[c(1:2,6:13,3:5),]

rm(MO_SP_Annual, MO_SP_Monthly, MO_SP)

phi<- merge(phi, data.frame(Parameter=phi_MO_sp$Parameter,
                            MO_spring=phi_MO_sp$Estimate),
            by="Parameter", all=TRUE)
phi<- phi[c(1,11,2:10,12:14),]




####################################################
#                                                  #
#       YELLOWSTONE RIVER SPRING YEARLINGS         #
#                                                  #
####################################################
YE_SP_Monthly<- extract_tables("./baseline-parameters/Rotella_2017_Update.pdf", 
                               pages=c(57))
YE_SP_Monthly<- as.data.frame(YE_SP_Monthly[[1]][4:nrow(YE_SP_Monthly[[1]]),])

names(YE_SP_Monthly)<- c("Start Date", "End Date", "Months", 
                         "Age at Interval Start (Months)", 
                         "Age at Interval End (Months)", 
                         "Monthly Survival", "Survival SE", 
                         "Proportion of Population Still Alive", 
                         "Proportion SE", 
                         "95% CI for Cumulative Proportion Surviving")



YE_SP_Annual<- extract_tables("./baseline-parameters/Rotella_2017_Update.pdf", 
                              pages=c(58))

YE_SP_Annual[[2]]<- NULL
YE_SP_Annual<- as.data.frame(YE_SP_Annual[[1]][4:nrow(YE_SP_Annual[[1]]), 
                                               2:ncol(YE_SP_Annual[[1]])])
names(YE_SP_Annual)<- c("Years Since Release", "Months Since Release", 
                        "Interval Survival Rate", "SE", "Release Type", "RPMA")



YE_SP<- extract_tables("./baseline-parameters/Rotella_2017_Update.pdf", 
                       pages=c(59, 61, 63, 64))
YE_SP<- lapply(1:length(YE_SP), function(x)
{
  if(x<6)
  {
    out<- as.data.frame(YE_SP[[x]][4:nrow(YE_SP[[x]]), 
                                   2:ncol(YE_SP[[x]])])
  }
  if(x==6 | x==7)
  {
    out<- as.data.frame(YE_SP[[x]][4:nrow(YE_SP[[x]]),])
  }
  if(x==8)
  {
    out<- as.data.frame(matrix(YE_SP[[x]][4,], nrow=1))
  }
  names(out)<- c("Cohort", "Type", "Start Date", "End Date", 
                 "Age at Interval Start (Months)", 
                 "Age at Interval End (Months)", 
                 "Proportion of Population Still Alive", 
                 "Proportion SE", "N at Interval End", 
                 "95% CI for N at Interval End")
  out$id<- 1:nrow(out)
  return(out)
})
YE_SP<- do.call(rbind, YE_SP)
YE_SP$Cohort<- as.numeric(as.character(YE_SP$Cohort))
YE_SP<- subset(YE_SP, Cohort %in% c(2,4,7:12))
YE_SP$`Start Date`<- strptime(YE_SP$`Start Date`, "%m/%d/%y")
YE_SP$`End Date`<- strptime(YE_SP$`End Date`, "%m/%d/%y")
YE_SP$`N at Interval End`<- as.numeric(gsub(",", "", as.character(YE_SP$`N at Interval End`)))
#YE_SP$Months<- as.numeric(as.character(YE_SP$`Age at Interval End (Months)`))-as.numeric(as.character(YE_SP$`Age at Interval Start (Months)`)) 
YE_SP$Days<- as.numeric(difftime(YE_SP$`End Date`, YE_SP$`Start Date`, 
                                 units="days"))
tmp<- data.frame(Cohort=c(2,4,7:12),
                 Release_Number=c(309, 1966, 5177, 1585, 373, 424, 316,
                                  755))
YE_SP<- merge(YE_SP, tmp, by="Cohort", all.x=TRUE)
rm(tmp)
YE_SP$`N at Interval Start`<- NA
YE_SP$`N at Interval Start`[2:nrow(YE_SP)]<- YE_SP$`N at Interval End`[1:(nrow(YE_SP)-1)]
YE_SP[which(YE_SP$id==1),]$`N at Interval Start`<- YE_SP[which(YE_SP$id==1),]$Release_Number
YE_SP$Interval_Proportion<- YE_SP$`N at Interval End`/YE_SP$`N at Interval Start`
#YE_SP$`Monthly Survival`<- YE_SP$Interval_Proportion^(1/YE_SP$Months)
YE_SP$`Daily Survival`<- YE_SP$Interval_Proportion^(1/YE_SP$Days)


phi_YE_sp<- lapply(unique(YE_SP$Cohort), function(x)
{
  dat<- YE_SP[which(YE_SP$Cohort==x),]
  min_year<- as.numeric(format(min(dat$`Start Date`), "%Y"))-1
  max_year<- as.numeric(format(max(dat$`Start Date`), "%Y"))
  birthdays<- strptime(paste0(min_year:max_year, "-06-01"), "%Y-%m-%d")
  if(length(birthdays)>=3)
  {
    params<- paste0("phi", c("_sp", 1:(length(birthdays)-2)))
  }
  if(length(birthdays)==2)
  {
    params<-"phi_sp"
  }
  vals<- sapply(2:length(birthdays), function(y)
  {
    tmp<-dat[which(dat$`Start Date`<=birthdays[y] & dat$`End Date`>birthdays[y-1]),]
    tmp$Days[nrow(tmp)]<- as.numeric(unname(difftime(birthdays[y], 
                                                     max(tmp$`Start Date`[nrow(tmp)],
                                                         birthdays[y-1]),
                                                     units = "days"))) 
    if(y>2 & nrow(tmp)>1)
    {
      tmp$Days[1]<- as.numeric(unname(difftime(tmp$`End Date`[1], birthdays[y-1], units = "days"))) 
    }
    surv<-prod(tmp$`Daily Survival`^tmp$Days)
    return(surv)
  })
  weights<- unique(dat$Release_Number)
  if(length(birthdays)>2)
  {
    for(i in 1:(length(vals)-1))
    {
      weights<- c(weights, weights[i]*vals[i])
    }
  }
  out<- data.frame(Cohort=x, 
                   Parameter=params,
                   Estimate=vals,
                   Numbers=weights)
  return(out)
})
phi_YE_sp<- do.call(rbind, phi_YE_sp)
phi_YE_sp$Weighted_Val<- phi_YE_sp$Estimate*phi_YE_sp$Numbers

phi_YE_sp<- ddply(phi_YE_sp, .(Parameter), summarize,
                  Estimate=sum(Weighted_Val)/sum(Numbers))
phi_YE_sp$River<- "YE"
phi_YE_sp$Type<- "spring yearling"
phi_YE_sp<- phi_YE_sp[c(1:2,5:12,3:4),]

phi<- merge(phi, data.frame(Parameter=phi_YE_sp$Parameter,
                            YE_spring=phi_YE_sp$Estimate),
            by="Parameter", all=TRUE)
phi<- phi[c(1,11,15,2:10,12:14,16:21),]

rm(YE_SP_Annual, YE_SP_Monthly, YE_SP)

####################################################
#                                                  #
#         MISSOURI RIVER SUMMER YEARLINGS          #
#                                                  #
####################################################
MO_SU_Monthly<- extract_tables("./baseline-parameters/Rotella_2017_Update.pdf", 
                               pages=c(68))
MO_SU_Monthly<- as.data.frame(MO_SU_Monthly[[1]][4:nrow(MO_SU_Monthly[[1]]),])

names(MO_SU_Monthly)<- c("Start Date", "End Date", "Months", 
                         "Age at Interval Start (Months)", 
                         "Age at Interval End (Months)", 
                         "Monthly Survival", "Survival SE", 
                         "Proportion of Population Still Alive", 
                         "Proportion SE", 
                         "95% CI for Cumulative Proportion Surviving")



MO_SU_Annual<- extract_tables("./baseline-parameters/Rotella_2017_Update.pdf", 
                              pages=c(69))


MO_SU_Annual<- as.data.frame(MO_SU_Annual[[1]][4:nrow(MO_SU_Annual[[1]]), 
                                               2:ncol(MO_SU_Annual[[1]])])
names(MO_SU_Annual)<- c("Years Since Release", "Months Since Release", 
                        "Interval Survival Rate", "SE", "Release Type", "RPMA")



MO_SU<- extract_tables("./baseline-parameters/Rotella_2017_Update.pdf", 
                       pages=c(70:78))
MO_SU[[12]]<- NULL
MO_SU[[6]]<- NULL
MO_SU<- lapply(1:length(MO_SU), function(x)
{
  if(x==1)
  {
    out<- as.data.frame(MO_SU[[x]][5:nrow(MO_SU[[x]]), 
                                   2:ncol(MO_SU[[x]])])
  }
  if(x>1 & x<9)
  {
    out<- as.data.frame(MO_SU[[x]][4:nrow(MO_SU[[x]]), 
                                   2:ncol(MO_SU[[x]])])
  }
  if(x>=9)
  {
    out<- as.data.frame(MO_SU[[x]][4:nrow(MO_SU[[x]]),])
  }
  names(out)<- c("Cohort", "Type", "Start Date", "End Date", 
                 "Age at Interval Start (Months)", 
                 "Age at Interval End (Months)", 
                 "Proportion of Population Still Alive", 
                 "Proportion SE", "N at Interval End", 
                 "95% CI for N at Interval End")
  out$id<- 1:nrow(out)
  return(out)
})
MO_SU<- do.call(rbind, MO_SU)
MO_SU$Cohort<- as.numeric(as.character(MO_SU$Cohort))
MO_SU<- subset(MO_SU, Cohort %in% c(1:5, 7:11))
MO_SU$`Start Date`<- strptime(MO_SU$`Start Date`, "%m/%d/%y")
MO_SU$`End Date`<- strptime(MO_SU$`End Date`, "%m/%d/%y")
MO_SU$`N at Interval End`<- as.numeric(gsub(",", "", as.character(MO_SU$`N at Interval End`)))
#MO_SU$Months<- as.numeric(as.character(MO_SU$`Age at Interval End (Months)`))-as.numeric(as.character(MO_SU$`Age at Interval Start (Months)`)) 
MO_SU$Days<- as.numeric(difftime(MO_SU$`End Date`, MO_SU$`Start Date`, 
                                 units="days"))
tmp<- data.frame(Cohort=c(1:5, 7:11),
                 Release_Number=c(295, 179, 1272, 2061, 896, 918, 688,
                                  2000, 809, 525))
MO_SU<- merge(MO_SU, tmp, by="Cohort", all.x=TRUE)
rm(tmp)
MO_SU$`N at Interval Start`<- NA
MO_SU$`N at Interval Start`[2:nrow(MO_SU)]<- MO_SU$`N at Interval End`[1:(nrow(MO_SU)-1)]
MO_SU[which(MO_SU$id==1),]$`N at Interval Start`<- MO_SU[which(MO_SU$id==1),]$Release_Number
MO_SU$Interval_Proportion<- MO_SU$`N at Interval End`/MO_SU$`N at Interval Start`
#MO_SU$`Monthly Survival`<- MO_SU$Interval_Proportion^(1/MO_SU$Months)
MO_SU$`Daily Survival`<- MO_SU$Interval_Proportion^(1/MO_SU$Days)


phi_MO_su<- lapply(unique(MO_SU$Cohort), function(x)
{
  dat<- MO_SU[which(MO_SU$Cohort==x),]
  min_year<- as.numeric(format(min(dat$`Start Date`), "%Y"))-1
  max_year<- as.numeric(format(max(dat$`Start Date`), "%Y"))
  birthdays<- strptime(paste0(min_year:max_year, "-06-01"), "%Y-%m-%d")
  if(length(birthdays)>=4)
  {
    params<- paste0("phi", c("_su", 2:(length(birthdays)-2)))
  }
  if(length(birthdays)==3)
  {
    params<-"phi_su"
  }
  vals<- sapply(3:length(birthdays), function(y)
  {
    tmp<-dat[which(dat$`Start Date`<=birthdays[y] & dat$`End Date`>birthdays[y-1]),]
    tmp$Days[nrow(tmp)]<- as.numeric(unname(difftime(birthdays[y], 
                                                     max(tmp$`Start Date`[nrow(tmp)],
                                                         birthdays[y-1]),
                                                         units = "days"))) 
    if(y>3 & nrow(tmp)>1)
    {
      tmp$Days[1]<- as.numeric(unname(difftime(tmp$`End Date`[1], birthdays[y-1], units = "days"))) 
    }
    surv<-prod(tmp$`Daily Survival`^tmp$Days)
    return(surv)
  })
  weights<- unique(dat$Release_Number)
  if(length(birthdays)>3)
  {
    for(i in 1:(length(vals)-1))
    {
      weights<- c(weights, weights[i]*vals[i])
    }
  }
  out<- data.frame(Cohort=x, 
                   Parameter=params,
                   Estimate=vals,
                   Numbers=weights)
  return(out)
})
phi_MO_su<- do.call(rbind, phi_MO_su)
phi_MO_su$Weighted_Val<- phi_MO_su$Estimate*phi_MO_su$Numbers


plot(phi_MO_su[which(phi_MO_su$Parameter=="phi3"),]$Cohort,
     phi_MO_su[which(phi_MO_su$Parameter=="phi3"),]$Estimate,
     ylim=c(min(phi_MO_su$Estimate, na.rm = TRUE),
            max(phi_MO_su$Estimate, na.rm = TRUE)))
points(phi_MO_su[which(phi_MO_su$Parameter=="phi4"),]$Cohort,
       phi_MO_su[which(phi_MO_su$Parameter=="phi4"),]$Estimate,
       col="red")
points(phi_MO_su[which(phi_MO_su$Parameter=="phi2"),]$Cohort,
       phi_MO_su[which(phi_MO_su$Parameter=="phi2"),]$Estimate,
       col="blue")

phi_MO_su<- ddply(phi_MO_su, .(Parameter), summarize,
                  Estimate=sum(Weighted_Val)/sum(Numbers))
phi_MO_su$River<- "MO"
phi_MO_su$Type<- "summer yearling"
phi_MO_su<- phi_MO_su[c(1,11:18,2:10),]

phi<- merge(phi, data.frame(Parameter=phi_MO_su$Parameter,
                            MO_summer=phi_MO_su$Estimate),
            by="Parameter", all=TRUE)
phi<- phi[c(1,11,2,15,3:10,12:14,16:21),]

rm(MO_SU_Annual, MO_SU_Monthly, MO_SU)



####################################################
#                                                  #
#       YELLOWSTONE RIVER SUMMER YEARLINGS         #
#                                                  #
####################################################
YE_SU_Monthly<- extract_tables("./baseline-parameters/Rotella_2017_Update.pdf", 
                               pages=c(79))
YE_SU_Monthly<- as.data.frame(YE_SU_Monthly[[1]][4:nrow(YE_SU_Monthly[[1]]),])

names(YE_SU_Monthly)<- c("Start Date", "End Date", "Months", 
                         "Age at Interval Start (Months)", 
                         "Age at Interval End (Months)", 
                         "Monthly Survival", "Survival SE", 
                         "Proportion of Population Still Alive", 
                         "Proportion SE", 
                         "95% CI for Cumulative Proportion Surviving")



YE_SU_Annual<- extract_tables("./baseline-parameters/Rotella_2017_Update.pdf", 
                              pages=c(80))


YE_SU_Annual<- as.data.frame(YE_SU_Annual[[1]][4:nrow(YE_SU_Annual[[1]]),])
names(YE_SU_Annual)<- c("Years Since Release", "Months Since Release", 
                        "Interval Survival Rate", "SE", "Release Type", "RPMA")



YE_SU<- extract_tables("./baseline-parameters/Rotella_2017_Update.pdf", 
                       pages=c(81,82,84,85, 87:89))
YE_SU<- lapply(1:length(YE_SU), function(x)
{
  if(x<8)
  {
    out<- as.data.frame(YE_SU[[x]][4:nrow(YE_SU[[x]]), 
                                   2:ncol(YE_SU[[x]])])
  }
  if(x>=8)
  {
    out<- as.data.frame(YE_SU[[x]][4:nrow(YE_SU[[x]]),])
  }
  names(out)<- c("Cohort", "Type", "Start Date", "End Date", 
                 "Age at Interval Start (Months)", 
                 "Age at Interval End (Months)", 
                 "Proportion of Population Still Alive", 
                 "Proportion SE", "N at Interval End", 
                 "95% CI for N at Interval End")
  out$id<- 1:nrow(out)
  return(out)
})
YE_SU<- do.call(rbind, YE_SU)
YE_SU$Cohort<- as.numeric(as.character(YE_SU$Cohort))
YE_SU<- subset(YE_SU, Cohort %in% c(1,2,4,5,7:11))
YE_SU$`Start Date`<- strptime(YE_SU$`Start Date`, "%m/%d/%y")
YE_SU$`End Date`<- strptime(YE_SU$`End Date`, "%m/%d/%y")
YE_SU$`N at Interval End`<- as.numeric(gsub(",", "", as.character(YE_SU$`N at Interval End`)))
#YE_SU$Months<- as.numeric(as.character(YE_SU$`Age at Interval End (Months)`))-as.numeric(as.character(YE_SU$`Age at Interval Start (Months)`)) 
YE_SU$Days<- as.numeric(difftime(YE_SU$`End Date`, YE_SU$`Start Date`, 
                                 units="days"))
tmp<- data.frame(Cohort=c(1,2,4,5,7:11),
                 Release_Number=c(486, 299, 1929, 676, 457, 2569, 1781,
                                  1077, 437))
YE_SU<- merge(YE_SU, tmp, by="Cohort", all.x=TRUE)
rm(tmp)
YE_SU$`N at Interval Start`<- NA
YE_SU$`N at Interval Start`[2:nrow(YE_SU)]<- YE_SU$`N at Interval End`[1:(nrow(YE_SU)-1)]
YE_SU[which(YE_SU$id==1),]$`N at Interval Start`<- YE_SU[which(YE_SU$id==1),]$Release_Number
YE_SU$Interval_Proportion<- YE_SU$`N at Interval End`/YE_SU$`N at Interval Start`
#YE_SU$`Monthly Survival`<- YE_SU$Interval_Proportion^(1/YE_SU$Months)
YE_SU$`Daily Survival`<- YE_SU$Interval_Proportion^(1/YE_SU$Days)


phi_YE_su<- lapply(unique(YE_SU$Cohort), function(x)
{
  dat<- YE_SU[which(YE_SU$Cohort==x),]
  min_year<- as.numeric(format(min(dat$`Start Date`), "%Y"))-1
  max_year<- as.numeric(format(max(dat$`Start Date`), "%Y"))
  birthdays<- strptime(paste0(min_year:max_year, "-06-01"), "%Y-%m-%d")
  if(length(birthdays)>=4)
  {
    params<- paste0("phi", c("_su", 2:(length(birthdays)-2)))
  }
  if(length(birthdays)==3)
  {
    params<-"phi_su"
  }
  vals<- sapply(3:length(birthdays), function(y)
  {
    tmp<-dat[which(dat$`Start Date`<=birthdays[y] & dat$`End Date`>birthdays[y-1]),]
    tmp$Days[nrow(tmp)]<- as.numeric(unname(difftime(birthdays[y], 
                                                     max(tmp$`Start Date`[nrow(tmp)],
                                                         birthdays[y-1]),
                                                     units = "days"))) 
    if(y>3 & nrow(tmp)>1)
    {
      tmp$Days[1]<- as.numeric(unname(difftime(tmp$`End Date`[1], birthdays[y-1], units = "days"))) 
    }
    surv<-prod(tmp$`Daily Survival`^tmp$Days)
    print(sum(tmp$Days))
    return(surv)
  })
  weights<- unique(dat$Release_Number)
  if(length(birthdays)>3)
  {
    for(i in 1:(length(vals)-1))
    {
      weights<- c(weights, weights[i]*vals[i])
    }
  }
  out<- data.frame(Cohort=x, 
                   Parameter=params,
                   Estimate=vals,
                   Numbers=weights)
  return(out)
})
phi_YE_su<- do.call(rbind, phi_YE_su)
phi_YE_su$Weighted_Val<- phi_YE_su$Estimate*phi_YE_su$Numbers

phi_YE_su<- ddply(phi_YE_su, .(Parameter), summarize,
                  Estimate=sum(Weighted_Val)/sum(Numbers))
phi_YE_su$River<- "YE"
phi_YE_su$Type<- "summer yearling"
phi_YE_su<- phi_YE_su[c(1,11:18,2:10),]

phi<- merge(phi, data.frame(Parameter=phi_YE_su$Parameter,
                            YE_summer=phi_YE_su$Estimate),
            by="Parameter", all=TRUE)
phi<- phi[c(1,11,2,15,3:10,12:14,16:21),]
phi$Age<- c(0, 0, 1, 1, 2:18)
#write.csv(phi, 
#          "./baseline-parameters/Rotella_2017_phi.csv",
#          row.names = FALSE)


rm(YE_SU_Annual, YE_SU_Monthly, YE_SU)

rm(phi_MO_fing, phi_MO_sp, phi_MO_su, phi_YE_fing, phi_YE_sp, phi_YE_su)


phi<- read.csv("./baseline-parameters/Rotella_2017_phi.csv")

par(mfrow=c(2,1))
plot(phi$Age[5:21], phi$MO_summer[5:21], xlab="Age", ylab="Survival",
     ylim=c(min(c(phi$MO_fing, phi$MO_spring, phi$MO_summer), na.rm = TRUE),
            1), col="red")
MO_summer<- lm(phi$MO_summer[5:21]~phi$Age[5:21])
abline(MO_summer$coefficients[1], MO_summer$coefficients[2], col="red")

points(2:12, phi$MO_spring[5:15])
#MO_spring<- lm(phi$MO_spring[5:15]~phi$Age[5:15])
#abline(MO_spring$coefficients[1], MO_spring$coefficients[2])
points(4:12, phi$MO_spring[7:15], col="green")
MO_spring<- lm(phi$MO_spring[7:15]~phi$Age[7:15])
abline(MO_spring$coefficients[1], MO_spring$coefficients[2], col="green")

points(2:9, phi$MO_fing[5:12], col="blue")
MO_fing<- lm(phi$MO_fing[5:12]~phi$Age[5:12])
abline(MO_fing$coefficients[1], MO_fing$coefficients[2], col="blue")

plot(1:9, phi$MO_fing[c(3,5:12)])
MO_fing2<- lm(phi$MO_fing[c(3,5:12)]~phi$Age[c(3,5:12)])
abline(MO_fing2$coefficients[1], MO_fing2$coefficients[2])

plot(MO_fing2$residuals)



plot(phi$Age[5:21], phi$YE_summer[5:21], xlab="Age", ylab="Survival",
     ylim=c(min(c(phi$YE_fing, phi$YE_spring, phi$YE_summer), na.rm = TRUE),
            1), col="red")
YE_summer<- lm(phi$YE_summer[5:21]~phi$Age[5:21])
abline(YE_summer$coefficients[1], YE_summer$coefficients[2], col="red")

points(2:11, phi$YE_spring[5:14])
#YE_spring<- lm(phi$YE_spring[5:14]~phi$Age[5:14])
#abline(YE_spring$coefficients[1], YE_spring$coefficients[2])
points(4:11, phi$YE_spring[7:14], col="green")
YE_spring<- lm(phi$YE_spring[7:14]~phi$Age[7:14])
abline(YE_spring$coefficients[1], YE_spring$coefficients[2], col="green")

points(2:9, phi$YE_fing[5:12], col="blue")
YE_fing<- lm(phi$YE_fing[5:12]~phi$Age[5:12])
abline(YE_fing$coefficients[1], YE_fing$coefficients[2], col="blue")


par(mfrow=c(3,1))
plot(phi$Age[5:12],phi$YE_fing[5:12], ylim=c(min(phi$YE_fing[5:12]),
                                             max(phi$MO_fing[5:12])))
points(phi$Age[5:12],phi$MO_fing[5:12], col="blue")



plot(phi$Age[5:15],phi$YE_spring[5:15], ylim=c(min(phi$YE_spring[5:14]),
                                             max(phi$MO_spring[5:15])))
points(phi$Age[5:15],phi$MO_spring[5:15], col="blue")



plot(phi$Age[5:21],phi$YE_summer[5:21], ylim=c(min(phi$YE_summer[5:21]),
                                               max(phi$MO_summer[5:21])))
points(phi$Age[5:21],phi$MO_summer[5:21], col="blue")
## RESULTS COULD BE FROM HOW THE MODEL WAS RAN(?)
