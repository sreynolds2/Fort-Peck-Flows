source("./R/1_global.r")

# # LOAD FLOW DATA
# flows<- read.xlsx("./dat/Fort_Peck_Flows_POR.xlsx", "POR", startRow=4)
# write.csv(flows, "./dat/Fort_Peck_Flows_POR.csv", row.names = FALSE)
# ## SUBSET TO THE MONTHS OF JUNE AND JULY
# flows$month<- as.numeric(format(flows$Date, "%m"))
# flows<- subset(flows, month==6 | month==7)
# flows<- flows[,1:8]
# write.csv(flows, "./dat/Spawning_Flows_POR.csv", row.names = FALSE)

# # LOAD SPILLWAY TEMPS
# spill<- read.xlsx("./dat/Fort_Peck_Flows_POR.xlsx", "Calcs", startRow=8, 
#                   colIndex=c(2,24:26))
# write.csv(spill, "./dat/Spawning_Spillway_Temps.csv", row.names = FALSE)



# # ANALYSES
# ## READ IN DATA
# flows<- read.csv("./dat/Spawning_Flows_POR.csv")
# flows$Date<- as.Date(flows$Date)
# spill<- read.csv("./dat/Spawning_Spillway_Temps.csv")
# spill$Date<- as.Date(spill$Date)
# 
# ## INPUT TEMPERATURE THRESHOLD AND PULL ASSOCIATED DATES
# temp_thshd<- c(16:18, 20)
# tbl<- expand.grid(Threshold_Temp=temp_thshd, 
#                   Weather_Pattern=c("Low", "Median", "High"))
# tbl$Threshold_Dates<- sapply(1:nrow(tbl), function(i)
# {
#   j<- ifelse(tbl$Weather_Pattern[i]=="Low", 1,
#              ifelse(tbl$Weather_Pattern[i]=="Median", 2, 3))
#   indx<- which(spill[,j+1]>tbl$Threshold_Temp[i])
#   if(length(indx)==0){out<- format(as.Date("1998-12-31"), "%m-%d")}
#   if(length(indx)>0)
#   {
#     indx<- min(indx)
#     out<- format(spill$Date[indx], "%m-%d")
#   }
#   return(out)
#   ## SAME DATA USED EACH YEAR
# })
# 
# ## DETERMINE YEARS AND DATES OF INTEREST BASED ON FLOWS
# ### FOR EACH YEAR, DETERMINE MAXIMUM JUNE AND JULY FLOWS 
# ### THAT ARE GREATER THAN 14000/0.8 (20% FROM SPILLWAY)
# ### DATES THAT ARE 3 DAYS POST PEAK AND CONFIRM IF
# ### TEMPERATURE CRITERIA IS MET
# ### AND DETERMIND DECREASING LEG DATE FOR WHEN HIGH, MEDIAN,
# ### AND LOW WEATHER PATTERNS PASS TEMPERATURE CRITERIA
# flows$Year<- as.numeric(format(flows$Date, "%Y"))
# yrs<- unique(flows$Year)
# flow_thshd<- c(17000, 14000/0.8, 18000)
# spawning_flow_info<- lapply(yrs, function(y)
# {
#   ## FIND MAXIMUM FLOWS GREATER THAN 14000/0.8
#   tmp<- subset(flows, Year==y)
#   out<- data.frame(Year=rep(y,7), Flow_Scenario=names(tmp)[2:8])
#   out$Max_Flow<- sapply(2:8, function(x)
#   {
#     max_flow<- max(tmp[,x])
#   })
#   out$Max_Date<- sapply(2:8, function(x)
#   {
#     indx<- max(which(tmp[,x]==out$Max_Flow[x-1]))
#     max_date<- format(tmp[indx,1], "%m-%d")
#     return(max_date)
#   })
#   out<- merge(out, data.frame(Flow_Threshold=flow_thshd))
#   out$Max_Flow_Test<- ifelse(out$Max_Flow>=out$Flow_Threshold, 1, 0)
#   out$Flow_Start<- sapply(1:nrow(out), function(x)
#   {
#     if(out$Max_Flow_Test[x]==0){fs<- out$Max_Date[x]}
#     if(out$Max_Flow_Test[x]==1)
#     {
#       col_num<- which(names(tmp)==out$Flow_Scenario[x])
#       indx<- min(which(tmp[,col_num]>= out$Flow_Threshold[x]))
#       fs<- format(tmp[indx,1], "%m-%d")
#     }
#     return(fs)
#   })
#   out$PP_Fail_Date<- sapply(1:nrow(out), function(x)
#   {
#     if(out$Max_Flow_Test[x]==0){pp<- out$Max_Date[x]}
#     if(out$Max_Flow_Test[x]==1)
#     {
#       col_num<- which(names(tmp)==out$Flow_Scenario[x])
#       indx<- max(which(tmp[,col_num]==out$Max_Flow[x])):nrow(tmp)
#       indx2<- which(tmp[indx,col_num]<out$Flow_Threshold[x])
#       if(length(indx2)==0){pp<- format(as.Date("1998-12-31"), "%m-%d")}
#       if(length(indx2)>0)
#       {
#         indx<- indx[min(indx2)]
#         pp<- format(tmp[indx,1], "%m-%d")
#       }
#     }
#     return(pp)
#   })
#   return(out)
# })
# spawning_flow_info<- do.call("rbind", spawning_flow_info) 
# spawning_flow_info<- spawning_flow_info[,c(1:3,5:7,4,8)]
# 
# spawning_info<- merge(spawning_flow_info, tbl)
# spawning_info$Overlap_Start<- sapply(1:nrow(spawning_info), function(x)
# {
#   if(spawning_info$Max_Flow_Test[x]==0){out<- format(as.Date("1998-12-31"), "%m-%d")}
#   if(spawning_info$Max_Flow_Test[x]==1)
#   {
#     if(max(spawning_info$Flow_Start[x], spawning_info$Threshold_Dates[x])>=spawning_info$PP_Fail_Date[x])
#     {
#       out<- format(as.Date("1998-12-31"), "%m-%d")
#     }
#     if(max(spawning_info$Flow_Start[x], spawning_info$Threshold_Dates[x])<spawning_info$PP_Fail_Date[x])
#     {
#       out<- max(spawning_info$Flow_Start[x], spawning_info$Threshold_Dates[x])
#     }
#   }
#   return(out)
# })
# spawning_info$Overlap_Test<- ifelse(spawning_info$Overlap_Start<format(as.Date("1998-08-01"), "%m-%d"), 1, 0)  
#   
# write.csv(spawning_info, "./dat/Spawning_Info_By_Year_And_Threshold.csv",
#           row.names = FALSE)

spawning_info<- read.csv("./dat/Spawning_Info_By_Year_And_Threshold.csv")


# ## THE FOLLOWING IS THE ORIGINAL CODE USED; 
# ## THE WEATHER PATTERNS HAVE SINCE BEEN UPDATED BY CRAIG AND ARE 
# ## STORED IN THE FILE TempClass Post 1930.xlsx -- WILL CLEAN AND UPDATE HERE
# # # DETERMINE WEATHER PATTERN FOR EACH YEAR
# # BC_temps<- read.csv("./dat/BC_POR.csv")
# # names(BC_temps)[1]<- "Date"
# # BC_temps$Date<- as.Date(BC_temps$Date)
# # write.csv(BC_temps, "./dat/Estimated_Annual_Temperatures.csv", row.names = FALSE)
# # tmp<- BC_temps[grep("-06-01", BC_temps$Date),]
# # tmp$Weather<- ifelse(tmp$Spillway %in% c(14.41, 14.67), "High",
# #                      ifelse(tmp$Spillway %in% c(12.67, 12.93), "Median",
# #                             "Low"))
# # tmp$Year<- format(tmp$Date, "%Y")
# # tmp<- tmp[, c("Year", "Weather")]
# # write.csv(tmp, "./dat/Observed_Annual_Weather.csv", row.names = FALSE)
# 
# weather<- read.csv("./dat/Observed_Annual_Weather.csv")
# temps<- read.csv("./dat/Estimated_Annual_Temperatures.csv")
#
# tmp<- read.xlsx("./dat/TempClass Post 1930.xlsx", "Classification2",
#                 stringsAsFactors = FALSE)
# names(tmp)[3]<- "Weather"
# tmp<- tmp[,c("Year", "Weather")]
# tmp[which(tmp$Weather=="Normal"),]$Weather<- "Median"
# write.csv(tmp, "./dat/Observed_Annual_Temperature_Class.csv", row.names = FALSE)
# diff<- which(weather$Weather!=tmp$Weather[1:nrow(weather)])
# yrs<- weather[diff, "Year"]
# rm(tmp, diff, yrs)
weather<- read.csv("./dat/Observed_Annual_Temperature_Class.csv")

# DETERMINE IF SPAWNING OCCURS GIVEN WEATHER PATTERN AND PP INFO
spawning_dates<- function(weather_data=NULL,
                          flow_threshold=17500,
                          temp_threshold=16,
                          post_peak=3)
{
  dat<- read.csv("./dat/Spawning_Info_By_Year_And_Threshold.csv")
  dat$Max_Date<- as.Date(paste0(dat$Year, "-", dat$Max_Date))
  dat$Overlap_Start<- as.Date(paste0(dat$Year, "-", dat$Overlap_Start))
  dat$PP_Fail_Date<- as.Date(paste0(dat$Year, "-", dat$PP_Fail_Date))
  dat<- subset(dat, Flow_Threshold==flow_threshold & Threshold_Temp==temp_threshold)
  if(nrow(dat)==0){return(print("The spawning information for the requested threshold 
                                combination is not available. Please update the spawning 
                                info table to obtain further output."))}
  indx<- which(weather_data$Year==2012)
  out<- lapply(1:indx, function(i)
  {
    tmp<- subset(dat, Year==weather_data$Year[i] & Weather_Pattern==weather_data$Weather[i])
    tmp$spawn_date<- format(as.Date(ifelse(tmp$Max_Date+post_peak>=tmp$Overlap_Start & 
                                      tmp$Max_Date+post_peak<tmp$PP_Fail_Date,
                                    tmp$Max_Date+post_peak, 
                                    ifelse(tmp$Overlap_Start>tmp$Max_Date+post_peak,
                                           tmp$Overlap_Start, as.Date("2000-12-31"))),
                             origin="1970-01-01"), "%m-%d")
    tmp$pp<- post_peak
    if(any(grepl("12-31", tmp$spawn_date)))
    {
      tmp[tmp$spawn_date=="12-31", ]$spawn_date<- "Fail"
      if(all(tmp$spawn_date=="Fail"))
      {
        tmp<- NULL
      }
    }
    return(data.frame(Year=tmp$Year, Flow_Scenario=tmp$Flow_Scenario,
                      Flow_Threshold=tmp$Flow_Threshold, 
                      Temp_Threshold=tmp$Threshold_Temp, 
                      Weather_Pattern=tmp$Weather_Pattern,
                      Min_Days_Post_Peak=tmp$pp,
                      Spawn_Date=tmp$spawn_date, Test=tmp$Overlap_Test))
  })
  out<- do.call("rbind", out)
  return(out)
}


sp1<- spawning_dates(weather_data = weather,
                     flow_threshold = 17500,
                     temp_threshold = 16,
                     post_peak = 3)

sp2<- spawning_dates(weather_data = weather,
                     flow_threshold = 17500,
                     temp_threshold = 17,
                     post_peak = 3)

sp3<- spawning_dates(weather_data = weather,
                     flow_threshold = 17500,
                     temp_threshold = 18,
                     post_peak = 3)

sp4<- spawning_dates(weather_data = weather,
                     flow_threshold = 17500,
                     temp_threshold = 20,
                     post_peak = 3)

sp<- rbind(sp1, sp2)
sp<- rbind(sp, sp3)
sp<- rbind(sp, sp4)

test<- dcast(sp, Year+Weather_Pattern+Flow_Scenario+Flow_Threshold+Min_Days_Post_Peak~Temp_Threshold,
             value.var = "Spawn_Date")
test[is.na(test)]<- "Fail"

indx<- which(sapply(1:nrow(test), function(i){all(test[i,6:9]=="Fail")}))
test<- test[-indx,]
write.csv(test, "./output/Spawn_Dates_by_Temperature.csv", 
          row.names = FALSE)


sp2<- spawning_dates(weather_data = weather,
                     flow_threshold = 17000,
                     temp_threshold = 16,
                     post_peak = 3)

sp3<- spawning_dates(weather_data = weather,
                     flow_threshold = 18000,
                     temp_threshold = 16,
                     post_peak = 3)

rm(sp4)
sp<- rbind(sp1, sp2)
sp<- rbind(sp, sp3)
test<- dcast(sp, Year+Weather_Pattern+Flow_Scenario+Temp_Threshold+Min_Days_Post_Peak~Flow_Threshold,
             value.var = "Spawn_Date")
test[is.na(test)]<- "Fail"
indx<- which(sapply(1:nrow(test), function(i){all(test[i,6:8]=="Fail")}))
test<- test[-indx,]
write.csv(test, "./output/Spawn_Dates_by_Flow_Threshold.csv", 
          row.names = FALSE)



sp2<- spawning_dates(weather_data = weather,
                     flow_threshold = 17500,
                     temp_threshold = 16,
                     post_peak = 1)

sp3<- spawning_dates(weather_data = weather,
                     flow_threshold = 17500,
                     temp_threshold = 16,
                     post_peak = 5)

sp<- rbind(sp1, sp2)
sp<- rbind(sp, sp3)
test<- dcast(sp, Year+Weather_Pattern+Flow_Scenario+Flow_Threshold+Temp_Threshold~Min_Days_Post_Peak,
             value.var = "Spawn_Date")
test[is.na(test)]<- "Fail"
indx<- which(sapply(1:nrow(test), function(i){all(test[i,6:8]=="Fail")}))
test<- test[-indx,]
write.csv(test, "./output/Spawn_Dates_by_Post_Peak_Days.csv", 
          row.names = FALSE)

test2<- spawning_info[,c("Year", "Weather_Pattern", "Flow_Scenario",
                         "Flow_Threshold", "Threshold_Temp", 
                         "Overlap_Start", "PP_Fail_Date", "Overlap_Test")]
test2<- subset(test2, Overlap_Test==1 & Flow_Threshold==17500 & 
                 Threshold_Temp==16)
names(test2)[5]<- "Temp_Threshold"
test2$Overlap_Start<- as.Date(paste0(test2$Year, "-", test2$Overlap_Start))
test2$PP_Fail_Date<- as.Date(paste0(test2$Year, "-", test2$PP_Fail_Date))
test2$Overlap_Last_Day<- test2$PP_Fail_Date-1

test2$Overlap_Start<- format(test2$Overlap_Start, "%m-%d")
test2$Overlap_Last_Day<- format(test2$Overlap_Last_Day, "%m-%d")
test2<- test2[,c(1:6,9)]

test2<- lapply(1:nrow(weather), function(i)
{
  tmp<- subset(test2, Year==weather$Year[i] & Weather_Pattern==weather$Weather[i])
  return(tmp)
})
test2<- do.call(rbind, test2)

test<- dcast(sp, Year+Weather_Pattern+Flow_Scenario+Flow_Threshold+Temp_Threshold~Min_Days_Post_Peak,
             value.var = "Spawn_Date")
test[is.na(test)]<- "Fail"

test3<- merge(test, test2, by=c("Year", "Weather_Pattern", "Flow_Scenario",
                                "Flow_Threshold", "Temp_Threshold"),
              all=TRUE)
test3[is.na(test3)]<- "Fail"
test3<- test3[,c(1:5,9,6:8,10)]
indx<- which(sapply(1:nrow(test3), function(i){all(test3[i,6:10]=="Fail")}))
test3<- test3[-indx,]
write.csv(test3, "./output/Spawn_Dates_Range.csv", 
          row.names = FALSE)

rm(sp, sp1, sp2, sp3, test, test2, test3)
tbl<- read.csv("./output/Spawn_Dates_by_Temperature.csv")
tbl<- tbl[, c(1:3,6:8)]
names(tbl)[4:6]<- c("Standard", "Temp_17","Temp_18")
tbl$Standard<- as.character(tbl$Standard)
tbl$Temp_17<- as.character(tbl$Temp_17)
tbl$Temp_18<- as.character(tbl$Temp_18)
tbl1<- read.csv("./output/Spawn_Dates_by_Flow_Threshold.csv")
tbl1<- tbl1[, c(1:3,6,8)]
names(tbl1)[4:5]<- c("Flow_17000", "Flow_18000")
tbl1$Flow_17000<- as.character(tbl1$Flow_17000)
tbl1$Flow_18000<- as.character(tbl1$Flow_18000)
tbl<- merge(tbl, tbl1, by=c("Year", "Weather_Pattern", "Flow_Scenario"), all=TRUE)
tbl1<- read.csv("./output/Spawn_Dates_Range.csv")
tbl1<- tbl1[, c(1:3,6:7,9:10)]
names(tbl1)[5:6]<- c("Days_PP_1", "Days_PP_5")
tbl1$Overlap_Start<- as.character(tbl1$Overlap_Start)
tbl1$Days_PP_1<- as.character(tbl1$Days_PP_1)
tbl1$Days_PP_5<- as.character(tbl1$Days_PP_5)
tbl1$Overlap_Last_Day<- as.character(tbl1$Overlap_Last_Day)
tbl<- merge(tbl, tbl1, by=c("Year", "Weather_Pattern", "Flow_Scenario"), all=TRUE)
rm(tbl1)
tbl[is.na(tbl)]<- "Fail"
write.csv(tbl, "./output/Spawn_Dates_Summary.csv", row.names = FALSE)

Spawn_List<- lapply(1:nrow(tbl), function(i)
{
  dts<- unique(unlist(tbl[i, c(4:8,10:11)]))
  dts<- dts[order(dts)]
  if(any(dts=="Fail")){dts[which(dts=="Fail")]<- ""}
  dts<- c(dts, rep("", 5-length(dts)))
  tmp<- tbl[i, c(1:3,9,12)]
  tmp$Spawn_Date_1<- dts[1]
  tmp$Spawn_Date_2<- dts[2]
  tmp$Spawn_Date_3<- dts[3]
  tmp$Spawn_Date_4<- dts[4]
  tmp$Spawn_Date_5<- dts[5]
  return(tmp)
})
Spawn_List<- do.call(rbind, Spawn_List)
write.csv(Spawn_List, "./output/Spawn_Dates_Summary2.csv", row.names = FALSE)

tbl<- read.csv("./output/Spawn_Dates_Summary.csv")
Spawn_List<- read.csv("./output/Spawn_Dates_Summary2.csv")

tmp<- Spawn_List[order(Spawn_List$Flow_Scenario),]
tmp<- tmp[,c(3,1:2,4:10)]


# OLD CODE
# ## FIND DATES THREE DAYS POST PEAK FLOW (AND CHECK FLOW CONDITION)
# peak_plus_three<-sapply(1:7, function(j)
# {
#     if(is.na(max_out[j]))
#     {
#       pp<- NA
#       check<- NA
#     }
#     if(!is.na(max_out[j]))
#     {
#       indx<- which(tmp[,j+1]==max_out[j])
#       if(length(indx)==1)
#       {
#         pp<- as.character(tmp[indx,"Date"]+3)
#         if(indx+3>nrow(tmp)){check<- NA}
#         if(indx+3<=nrow(tmp)){check<- tmp[indx+3, j+1]>14000/0.8}
#       }
#       if(length(indx)>1)
#       {
#         if(all(indx %in% indx[1]:(indx[1]+length(indx)-1)))
#         {
#           pp<- as.character(tmp[indx[length(indx)],"Date"]+3)
#           if(indx[length(indx)]+3>nrow(tmp)){check<- NA}
#           if(indx[length(indx)]+3<=nrow(tmp))
#           {
#             check<- tmp[indx[length(indx)]+3, j+1]>14000/0.8
#           }
#         }
#         if(!all(indx %in% indx[1]:(indx[1]+length(indx)-1)))
#         {
#           pp<- "Multiple Peak Flows"
#           check<- NA
#         }
#       }
#     }
#     return(rbind(pp, check))
#   })
#   out$Post_Peak_Date<- peak_plus_three[1,]
#   out$Post_Peak_Flows<- peak_plus_three[2,]
#   High_Date<- tbl[which(tbl$Weather_Pattern=="High"),"Threshold_Dates"] 
#   High_Date<- as.Date(paste0(y, "-", High_Date))
#   Median_Date<- tbl[which(tbl$Weather_Pattern=="Median"),"Threshold_Dates"] 
#   Median_Date<- as.Date(paste0(y, "-", Median_Date))
#   Low_Date<- tbl[which(tbl$Weather_Pattern=="Low"),"Threshold_Dates"] 
#   Low_Date<- as.Date(paste0(y, "-", Low_Date))
#   high_med_low<-sapply(1:7, function(j)
#   {
#     if(is.na(max_out[j]))
#     {
#       spds<- c(NA, NA, NA)
#     }
#     if(!is.na(max_out[j]))
#     {
#       # GATHER INFO BASED ON HIGH TEMPERATURE DATE
#       tmp2<- subset(tmp, Date>=High_Date)
#       indx<- which(tmp2[,j+1]>14000/0.8)
#       if(length(indx)==0)
#       {
#         spds<- c(NA, NA, NA)
#       }
#       if(length(indx)>0)
#       {
#         if(max(indx)==nrow(tmp2))
#         {
#           if(tmp2[nrow(tmp2),j+1]==max(tmp2[,j+1]))
#           {
#             spds<- c("Increasing Flows", "Increasing Flows", "Increasing Flows")
#           }
#           if(tmp2[nrow(tmp2),j+1]!=max(tmp2[,j+1]))
#           {
#             h<- tmp2[indx[min(which(tmp2[indx[1:(length(indx)-1)], j+1]>tmp2[indx[1:(length(indx)-1)]+1, j+1]))],"Date"]
#           }
#         }
#         if(max(indx)!=nrow(tmp2))
#         {
#           h<- tmp2[indx[min(which(tmp2[indx, j+1]>tmp2[indx+1, j+1]))],"Date"]
#         }
#         if(exists("h"))
#         {
#           if(h>=Low_Date)
#           {
#             spds<- c(as.character(h), as.character(h), as.character(h))
#           }
#           if(h<Low_Date & h>=Median_Date)
#           {
#             spds<- c(as.character(h), as.character(h), "run")
#           }
#           if(h<Median_Date)
#           {
#             spds<- c(as.character(h), "run", "run")
#           }
#           rm(h)
#         }
#       }
#       # GATHER MORE INFO, IF NEEDED, ON MEDIAN TEMPERATURE DATE 
#       if(!is.na(spds[2]) & spds[2]=="run")
#       {
#         tmp2<- subset(tmp, Date>=Median_Date)
#         indx<- which(tmp2[,j+1]>14000/0.8)
#         if(length(indx)==0)
#         {
#           spds[2:3]<- c(NA, NA)
#         }
#         if(length(indx)>0)
#         {
#           if(max(indx)==nrow(tmp2))
#           {
#             if(tmp2[nrow(tmp2),j+1]==max(tmp2[,j+1]))
#             {
#               spds[2:3]<- c("Increasing Flows", "Increasing Flows")
#             }
#             if(tmp2[nrow(tmp2),j+1]!=max(tmp2[,j+1]))
#             {
#               m<- tmp2[indx[min(which(tmp2[indx[1:(length(indx)-1)], j+1]>tmp2[indx[1:(length(indx)-1)]+1, j+1]))],"Date"]
#             }
#           }
#           if(max(indx)!=nrow(tmp2))
#           {
#             m<- tmp2[indx[min(which(tmp2[indx, j+1]>tmp2[indx+1, j+1]))],"Date"]
#           }
#           if(exists("m"))
#           {
#             if(m>=Low_Date)
#             {
#               spds[2:3]<- c(as.character(m), as.character(m))
#             }
#             if(m<Low_Date)
#             {
#               spds[2]<- as.character(m)
#             }
#             rm(m)
#           }
#         }
#       }
#       # GATHER MORE INFO, IF NEEDED, ON LOW TEMPERATURE DATE
#       if(!is.na(spds[3]) & spds[3]=="run")
#       {
#         tmp2<- subset(tmp, Date>=Low_Date)
#         indx<- which(tmp2[,j+1]>14000/0.8)
#         if(length(indx)==0)
#         {
#           spds[3]<- NA
#         }
#         if(length(indx)>0)
#         {
#           if(max(indx)==nrow(tmp2))
#           {
#             if(tmp2[nrow(tmp2),j+1]==max(tmp2[,j+1]))
#             {
#               spds[3]<- "Increasing Flows"
#             }
#             if(tmp2[nrow(tmp2),j+1]!=max(tmp2[,j+1]))
#             {
#               spds[3]<- as.character(tmp2[indx[min(which(tmp2[indx[1:(length(indx)-1)], j+1]>tmp2[indx[1:(length(indx)-1)]+1, j+1]))],"Date"])
#             }
#           }
#           if(max(indx)!=nrow(tmp2))
#           {
#             spds[3]<- as.character(tmp2[indx[min(which(tmp2[indx, j+1]>tmp2[indx+1, j+1]))],"Date"])
#           }
#         }
#       }
#     }
#     return(spds)
#   })
#   out$Low_Date<- high_med_low[3,]
#   out$Median_Date<- high_med_low[2,]
#   out$High_Date<- high_med_low[1,]
#   thsd<- sapply(1:7, function(j)
#   {
#     c(tmp[which(tmp$Date==Low_Date),j+1]>14000/0.8,
#       tmp[which(tmp$Date==Median_Date),j+1]>14000/0.8, 
#       tmp[which(tmp$Date==High_Date),j+1]>14000/0.8)
#   })
#   out$Low_Tshd<- thsd[1,]
#   out$Median_Tshd<- thsd[2,]
#   out$High_Tshd<- thsd[3,]
#   return(out)
# })
# spawning_info<- do.call(rbind, spawning_info)
# spawning_info$Post_Peak_Flows<- as.logical(spawning_info$Post_Peak_Flows)
# write.csv(spawning_info, "./output/Spawning_Info.csv", row.names=FALSE)
# 
# 
# ### ELIMINATE YEARS IN WHICH NO ALTERNATIVE FLOW SURPASSES 
# ### 14000/0.8 (20% FROM SPILLWAY)
# max_flows<- dcast(spawning_info, Year~Flow_Scenario, value.var = "Max_Flows")
# max_flows<- max_flows[-which(sapply(1:nrow(max_flows), function(i){all(is.na(max_flows[i,2:8]))})),]
# write.csv(max_flows, "./dat/Spawning_Viable_Max_Flows.csv", row.names = FALSE)
# 
# low<- dcast(spawning_info, Year~Flow_Scenario, value.var = "Low_Date")
# low<- low[-which(sapply(1:nrow(low), function(i){all(is.na(low[i,2:8]))})),]
# 
# test<- data.frame(Med=as.Date(spawning_info$Median_Date),
#                   Peak3=as.Date(spawning_info$Post_Peak_Date),
#                   Post=as.Date(spawning_info$Median_Date)>=as.Date(spawning_info$Post_Peak_Date),
#                   Check=spawning_info$Post_Peak_Flows)
# indx<- which(sapply(1:nrow(test), function(i){all(is.na(test[i,]))}))
# test<- test[-indx,]
# 
# ## AT SOME POINT, COMPUTE BELOW DAM TEMPERATURES BY DAY, WEATHER, 
# ## AND ALTERNATIVE
# 
# ## HOW RAPIDLY DO WE EXPECT THE TEMPERATURES TO CHANGE WHEN WARM
# ## STREAM IS AVAILABLE VS. WHEN FLOWS DECREASE BELOW THRESHOLD
# ## WHAT REACTION DO WE EXPECT STURGEON WILL HAVE TO POTENTIALLY RAPIDLY
# ## DECREASING TEMPS WHEN THIS OCCURS
# 
# 
