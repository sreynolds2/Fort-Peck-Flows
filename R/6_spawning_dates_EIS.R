source("./R/1_global.r")

# # CHECK THAT FLOW DATA IS THE SAME
# flows<- read.csv("./dat/Spawning_Flows_POR.csv", stringsAsFactors = FALSE)
# flows$Date<- as.Date(flows$Date)
# #
# # flows2<- read.xlsx("C:/Users/sreynolds/Desktop/Dropbox Files/temps and spawn dates/BC_CTE_POR_R3.xlsx", "Spill_Apron_Temps",
# #                    rows=c(1,60:30317), cols=c(1,10:16))
# # flows2$Date<- as.Date(flows2$Date, origin="1899-12-30")
# # flows2$month<- as.numeric(format(flows2$Date, "%m"))
# # flows2<- subset(flows2, month==6 | month==7)
# # flows2<- flows2[,1:8]
# # all(flows$Date==flows2$Date)
# # max(abs(flows[,2:8]-flows2[,2:8]))
# 
# # # COMPARE SPILLWAY TEMP DATA
# # ## ORIGINAL
# # spill<- read.csv("./dat/Spawning_Spillway_Temps.csv", stringsAsFactors = FALSE)
# # spill$Date<- as.Date(spill$Date)
# # spill<- subset(spill, format(spill$Date, "%m-%d")<"08-01")
# # spill$Year<- format(spill$Date, "%Y")
# # names(spill)[3]<- "Spill.Median"
# # temp_dat<- read.csv("./dat/Observed_Annual_Temperature_Class.csv")
# #
# # spill_temps<- expand.grid(Date=format(spill$Date, "%m-%d"), Year=1930:2012)
# # spill_temps$Date<- as.Date(paste0(spill_temps$Year,"-",spill_temps$Date))
# # spill_temps$Temp<- 0
# # for(i in 1:nrow(spill_temps))
# # {
# #   cls<- temp_dat[which(temp_dat$Year==spill_temps$Year[i]),]$Weather
# #   rw<- which(format(spill$Date, "%m-%d")==format(spill_temps[i,]$Date, "%m-%d"))
# #   spill_temps$Temp[i]<- spill[rw,paste0("Spill.", cls)]
# # }
# # spill<- spill_temps
# # rm(spill_temps, cls, i, rw)
# # spill$No.Act<- ifelse(flows$No.Act>=17500, spill$Temp, NA)
# # spill$Alt.1<- ifelse(flows$Alt1>=17500, spill$Temp, NA)
# # spill$Alt.1a<- ifelse(flows$Alt.1a>=17500, spill$Temp, NA)
# # spill$Alt.1b<- ifelse(flows$Alt.1b>=17500, spill$Temp, NA)
# # spill$Alt.2<- ifelse(flows$Alt.2>=17500, spill$Temp, NA)
# # spill$Alt.2a<- ifelse(flows$Alt.2a>=17500, spill$Temp, NA)
# # spill$Alt.2b<- ifelse(flows$Alt.2b>=17500, spill$Temp, NA)
# # spill<- spill[,c(1,4:10)]
# # indx<- NULL
# # for(i in 1:nrow(spill))
# # {
# #   if(all(is.na(spill[i,2:8])))
# #   {
# #     indx<- c(indx,i)
# #   }
# # }
# # spill<- spill[-indx,]
# #
# # ## PULL NEW DATA
# # spill2<- read.xlsx("C:/Users/sreynolds/Desktop/Dropbox Files/temps and spawn dates/BC_CTE_POR_R3.xlsx", "Spill_Apron_Temps",
# #                    rows=c(1,60:30317), cols=c(1:8))
# # spill2$Date<- as.Date(spill2$Date, origin="1899-12-30")
# # ## REDUCE TO DESIRED MONTHS
# # spill2$month<- format(spill2$Date, "%m-%d")
# # spill2<- subset(spill2, month>="06-01" & month<"08-01")
# # ## LOOK AT ONLY SPILLS OVER SPILLWAY
# # spill2$No.Act<- ifelse(flows$No.Act>=17500, spill2$AT.No.Act, NA)
# # spill2$Alt.1<- ifelse(flows$Alt1>=17500, spill2$AT.Alt1, NA)
# # spill2$Alt.1a<- ifelse(flows$Alt.1a>=17500, spill2$AT.Alt.1a, NA)
# # spill2$Alt.1b<- ifelse(flows$Alt.1b>=17500, spill2$AT.Alt.1b, NA)
# # spill2$Alt.2<- ifelse(flows$Alt.2>=17500, spill2$AT.Alt.2, NA)
# # spill2$Alt.2a<- ifelse(flows$Alt.2a>=17500, spill2$AT.Alt.2a, NA)
# # spill2$Alt.2b<- ifelse(flows$Alt.2b>=17500, spill2$AT.Alt.2b, NA)
# # spill2<- spill2[,c(1,10:16)]
# # indx<- NULL
# # for(i in 1:nrow(spill2))
# # {
# #   if(all(is.na(spill2[i,2:8])))
# #   {
# #     indx<- c(indx,i)
# #   }
# # }
# # spill2<- spill2[-indx,]
# # rm(i, indx)
# #
# # diff<- spill2[,2:8]-spill[,2:8]
# # max(diff, na.rm = TRUE)
# # min(diff, na.rm = TRUE)
# # diff$Date<- spill$Date
# 
# spills<- read.xlsx("C:/Users/sreynolds/Desktop/Dropbox Files/temps and spawn dates/BC_CTE_POR_R3.xlsx",
#                    "BC_Temp_POR", cols=c(1,3))
# spills$Date<- as.Date(spills$Date, origin="1899-12-30")
# ## REDUCE TO DESIRED MONTHS
# spills$month<- format(spills$Date, "%m-%d")
# spills<- subset(spills, month>="06-01" & month<"08-01")
# 
# ## INPUT TEMPERATURE THRESHOLD AND PULL ASSOCIATED DATES
# temp_thshd<- c(16:18, 20)
# yrs<- 1930:2012 #unique(as.numeric(format(spill2$Date, "%Y")))
# tbl<- expand.grid(Year=yrs, Threshold_Temp=temp_thshd)
# # spill2$Temp<- 0
# # for(i in 1:nrow(spill2))
# # {
# #   spill2$Temp[i]<- max(spill2[i,2:8], na.rm = TRUE)
# # }
# tbl$Threshold_Dates<- sapply(1:nrow(tbl), function(i)
# {
#   #tmp<- subset(spill2, format(spill2$Date, "%Y")==tbl$Year[i])
#   #indx<- which(tmp$Temp>tbl$Threshold_Temp[i])
#   tmp<- subset(spills, format(spills$Date, "%Y")==tbl$Year[i])
#   indx<- which(tmp$Spillway>tbl$Threshold_Temp[i])
#   if(length(indx)==0){out<- as.Date(paste0(tbl$Year[i], "-12-31"))}
#   if(length(indx)>0)
#   {
#     indx<- min(indx)
#     out<- tmp$Date[indx]
#   }
#   return(out)
# })
# tbl$Threshold_Dates<- as.Date(tbl$Threshold_Dates, origin="1970-01-01")
# 
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
#     if(max(spawning_info$Flow_Start[x], format(spawning_info$Threshold_Dates[x], "%m-%d"))>=spawning_info$PP_Fail_Date[x])
#     {
#       out<- format(as.Date("1998-12-31"), "%m-%d")
#     }
#     if(max(spawning_info$Flow_Start[x], format(spawning_info$Threshold_Dates[x], "%m-%d"))<spawning_info$PP_Fail_Date[x])
#     {
#       out<- max(spawning_info$Flow_Start[x], format(spawning_info$Threshold_Dates[x], "%m-%d"))
#     }
#   }
#   return(out)
# })
# spawning_info$Overlap_Test<- ifelse(spawning_info$Overlap_Start<format(as.Date("1998-08-01"), "%m-%d"), 1, 0)
# 
# write.csv(spawning_info, "./dat/Spawning_Info_By_Year_And_Threshold_NEW.csv",
#           row.names = FALSE)

spawning_info<- read.csv("./dat/Spawning_Info_By_Year_And_Threshold_NEW.csv")
spawning_info$Max_Date<- as.Date(paste0(spawning_info$Year, "-", spawning_info$Max_Date))
spawning_info$Overlap_Start<- as.Date(paste0(spawning_info$Year, "-", spawning_info$Overlap_Start))
spawning_info$PP_Fail_Date<- as.Date(paste0(spawning_info$Year, "-", spawning_info$PP_Fail_Date))
weather<- read.csv("./dat/Observed_Annual_Temperature_Class.csv")

# DETERMINE IF SPAWNING OCCURS GIVEN WEATHER PATTERN AND PP INFO
spawning_dates<- function(spn_data=NULL,
                          weather_data=NULL,
                          flow_threshold=17500,
                          temp_threshold=16,
                          post_peak=3)
{
  dat<- subset(spn_data, Flow_Threshold==flow_threshold & Threshold_Temp==temp_threshold)
  if(nrow(dat)==0){return(print("The spawning information for the requested threshold 
                                combination is not available. Please update the spawning 
                                info table to obtain further output."))}
  indx<- which(weather_data$Year==2012)
  out<- lapply(1:indx, function(i)
  {
    tmp<- subset(dat, Year==weather_data$Year[i])
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
                      Weather_Pattern=rep(weather_data$Weather[i], length(tmp$Year)),
                      Min_Days_Post_Peak=tmp$pp,
                      Spawn_Date=tmp$spawn_date, Test=tmp$Overlap_Test))
  })
  out<- do.call("rbind", out)
  return(out)
}


sp1<- spawning_dates(spn_data = spawning_info,
                     weather_data = weather,
                     flow_threshold = 17500,
                     temp_threshold = 16,
                     post_peak = 3)

sp2<- spawning_dates(spn_data = spawning_info,
                     weather_data = weather,
                     flow_threshold = 17500,
                     temp_threshold = 17,
                     post_peak = 3)

sp3<- spawning_dates(spn_data = spawning_info,
                     weather_data = weather,
                     flow_threshold = 17500,
                     temp_threshold = 18,
                     post_peak = 3)

sp4<- spawning_dates(spn_data = spawning_info,
                     weather_data = weather,
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
write.csv(test, "./output/Spawn_Dates_by_Temperature_NEW.csv", 
          row.names = FALSE)


sp2<- spawning_dates(spn_data = spawning_info,
                     weather_data = weather,
                     flow_threshold = 17000,
                     temp_threshold = 16,
                     post_peak = 3)

sp3<- spawning_dates(spn_data = spawning_info,
                     weather_data = weather,
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
write.csv(test, "./output/Spawn_Dates_by_Flow_Threshold_NEW.csv", 
          row.names = FALSE)



sp2<- spawning_dates(spn_data = spawning_info,
                     weather_data = weather,
                     flow_threshold = 17500,
                     temp_threshold = 16,
                     post_peak = 1)

sp3<- spawning_dates(spn_data = spawning_info,
                     weather_data = weather,
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
write.csv(test, "./output/Spawn_Dates_by_Post_Peak_Days_NEW.csv", 
          row.names = FALSE)

test2<- spawning_info[,c("Year", "Flow_Scenario", "Flow_Threshold", 
                         "Threshold_Temp", "Overlap_Start", 
                         "PP_Fail_Date", "Overlap_Test")]
test2<- subset(test2, Overlap_Test==1 & Flow_Threshold==17500 & 
                 Threshold_Temp==16)
names(test2)[4]<- "Temp_Threshold"
test2$Overlap_Last_Day<- test2$PP_Fail_Date-1

test2$Overlap_Start<- format(test2$Overlap_Start, "%m-%d")
test2$Overlap_Last_Day<- format(test2$Overlap_Last_Day, "%m-%d")
test2<- test2[,c(1:5,8)]

test<- dcast(sp, Year+Weather_Pattern+Flow_Scenario+Flow_Threshold+Temp_Threshold~Min_Days_Post_Peak,
             value.var = "Spawn_Date")
test[is.na(test)]<- "Fail"

test3<- merge(test, test2, by=c("Year", "Flow_Scenario",
                                "Flow_Threshold", "Temp_Threshold"),
              all=TRUE)
test3[is.na(test3)]<- "Fail"
test3<- test3[,c(1,5,2:4,9,6:8,10)]
indx<- which(sapply(1:nrow(test3), function(i){all(test3[i,6:10]=="Fail")}))
test3<- test3[-indx,]
write.csv(test3, "./output/Spawn_Dates_Range_NEW.csv", 
          row.names = FALSE)

rm(sp, sp1, sp2, sp3, test, test2, test3)
tbl<- read.csv("./output/Spawn_Dates_by_Temperature_NEW.csv")
tbl<- tbl[, c(1:3,6:8)]
names(tbl)[4:6]<- c("Standard", "Temp_17","Temp_18")
tbl$Standard<- as.character(tbl$Standard)
tbl$Temp_17<- as.character(tbl$Temp_17)
tbl$Temp_18<- as.character(tbl$Temp_18)
tbl1<- read.csv("./output/Spawn_Dates_by_Flow_Threshold_NEW.csv")
tbl1<- tbl1[, c(1:3,6,8)]
names(tbl1)[4:5]<- c("Flow_17000", "Flow_18000")
tbl1$Flow_17000<- as.character(tbl1$Flow_17000)
tbl1$Flow_18000<- as.character(tbl1$Flow_18000)
tbl<- merge(tbl, tbl1, by=c("Year", "Weather_Pattern", "Flow_Scenario"), all=TRUE)
tbl1<- read.csv("./output/Spawn_Dates_Range_NEW.csv")
tbl1<- tbl1[, c(1:3,6:7,9:10)]
names(tbl1)[5:6]<- c("Days_PP_1", "Days_PP_5")
tbl1$Overlap_Start<- as.character(tbl1$Overlap_Start)
tbl1$Days_PP_1<- as.character(tbl1$Days_PP_1)
tbl1$Days_PP_5<- as.character(tbl1$Days_PP_5)
tbl1$Overlap_Last_Day<- as.character(tbl1$Overlap_Last_Day)
tbl<- merge(tbl, tbl1, by=c("Year", "Weather_Pattern", "Flow_Scenario"), all=TRUE)
rm(tbl1)
tbl[is.na(tbl)]<- "Fail"
write.csv(tbl, "./output/Spawn_Dates_Summary_NEW.csv", row.names = FALSE)


# CHECK DATE NUMBERS
# Spawn_List<- lapply(1:nrow(tbl), function(i)
# {
#   dts<- unique(unlist(tbl[i, c(4:8,10:11)]))
#   if(any(dts=="Fail")){dts<- dts[-which(dts=="Fail")]}
#   dts<- dts[order(dts)]
#   return(length(dts))
# })
# max(unlist(Spawn_List))
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
write.csv(Spawn_List, "./output/Spawn_Dates_Summary2_NEW.csv", row.names = FALSE)

# GENERATE HATCH DATE TABLE LIKE CRAIG'S
## READ IN SUMMARY TABLE
tbl<- read.csv("./output/Spawn_Dates_Summary_NEW.csv")
### SUBSET TO STANDARD DATES
tbl<- tbl[-which(tbl$Standard=="Fail"),]
tbl$Hatch<- format(as.Date(paste0(tbl$Year, "-", tbl$Standard), 
                           "%Y-%m-%d")+7, "%m-%d")
test<- dcast(tbl, Year+Weather_Pattern~Flow_Scenario, value.var = "Hatch")
test[is.na(test)]<- ""
test<- test[,c(1,2,8,3:7,9)]

# NEW TEMPERATURE DATA
tbl<- read.csv("./output/Spawn_Dates_Summary_NEW.csv")
indx<- which(tbl$Temp_17!=tbl$Standard & tbl$Temp_17!="Fail")
temps17<- tbl[indx,c(1:6)]
temps17<- temps17[order(temps17$Year, temps17$Flow_Scenario),]
new_temps<- dcast(temps17, Year+Weather_Pattern~Flow_Scenario, 
                  value.var="Temp_17")
new_temps[is.na(new_temps)]<- ""

indx<- which(tbl$Temp_18!=tbl$Standard & tbl$Temp_18!=tbl$Temp_17 & 
               tbl$Temp_18!="Fail")
temps18<- tbl[indx,c(1:6)]
tmp<- dcast(temps18, Year+Weather_Pattern~Flow_Scenario, 
            value.var="Temp_18")
tmp[is.na(tmp)]<- ""
new_temps<- rbind(new_temps, tmp)
new_temps<- new_temps[order(new_temps$Year),c(1,2,8,3:7,9)] 
write.csv(new_temps, "./output/Additional_Temperature_Dates_EIS.csv", row.names = FALSE)

rm(tmp, temps17, temps18)

## HATCH TABLE
hatchT<- new_temps[,c(1,3:9,2)]
for(i in 1:nrow(hatchT))
{
  for(j in 2:8)
  {
    hatchT[i,j]<- ifelse(hatchT[i,j]=="", "",
                        format(as.Date(paste0(hatchT$Year[i], "-", 
                                              hatchT[i,j]))+7, "%m-%d"))
  }
}
write.csv(hatchT, "./output/Additional_Temperature_Hatch_EIS.csv", row.names = FALSE)


# NEW FLOW DATA
indx<- which(tbl$Flow_17000!=tbl$Standard & tbl$Flow_17000!="Fail")
indx<- c(indx, which(tbl$Flow_18000!=tbl$Standard & 
                      tbl$Flow_18000!=tbl$Flow_17000 & 
                      tbl$Flow_18000!="Fail"))
new_flows<- tbl[indx,c(1:4,7:8)]
new_flows<- dcast(new_flows, Year+Weather_Pattern~Flow_Scenario, 
                  value.var="Flow_17000")
new_flows$Alt.1b<- ""
new_flows$Alt.2<- ""
new_flows$Alt.2a<- ""
new_flows$Alt.2b<- ""
new_flows$Alt.NoAct<- ""
new_flows$Alt.1<- ""
new_flows<- new_flows[,c(1,2,9,3:8)]
write.csv(new_flows, "./output/Additional_Flow_Dates_EIS.csv", row.names = FALSE)

# NEW POST-PEAK DATA
indx<- which(tbl$Days_PP_1!=tbl$Standard & tbl$Days_PP_1!="Fail")
pp1<- tbl[indx,c(1:4,10,11)]
pp1<- pp1[order(pp1$Year, pp1$Flow_Scenario),]
new_pp<- dcast(pp1, Year+Weather_Pattern~Flow_Scenario, 
                  value.var="Days_PP_1")
new_pp[is.na(new_pp)]<- ""

indx<- which(tbl$Days_PP_5!=tbl$Standard & 
               tbl$Days_PP_5!=tbl$Days_PP_1 & 
               tbl$Days_PP_5!="Fail")
pp5<- tbl[indx,c(1:4,10,11)]
tmp<- dcast(pp5, Year+Weather_Pattern~Flow_Scenario, 
            value.var="Days_PP_5")
tmp[is.na(tmp)]<- ""
new_pp<- rbind(new_pp, tmp)
new_pp<- new_pp[order(new_pp$Year),c(1,2,8,3:7,9)] 
write.csv(new_pp, "./output/Additional_PostPeak_Dates_EIS.csv", row.names = FALSE)
rm(indx, tmp, pp1, pp5)

## HATCH TABLE
hatchPP<- new_pp[,c(1,3:9,2)]
for(i in 1:nrow(hatchPP))
{
  for(j in 2:8)
  {
    hatchPP[i,j]<- ifelse(hatchPP[i,j]=="", "",
                          format(as.Date(paste0(hatchPP$Year[i], "-", 
                                                hatchPP[i,j]))+7, "%m-%d"))
  }
}
write.csv(hatchPP, "./output/Additional_PostPeak_Hatch_EIS.csv", row.names = FALSE)



##  1975, 76, 97:  SPAWN ON 4th DAY  ("PEAK" + 3) OF CONSTANT POST-16 DEGREES 
### PLOT EXAMPLE
tmp<- subset(flows, Date<"1975-08-01" & Date>"1975-05-31")
tmp2<- subset(flows, Date<"1976-08-01" & Date>"1976-05-31")
tmp3<- subset(flows, Date<"1997-08-01" & Date>"1997-05-31")
plot(as.Date(paste0("2021-",format(tmp$Date, "%m-%d"))), tmp$Alt.1b, type="l", 
     xlab="Date", ylab="Q (cfs)", main="Alterntive 1b")
points(as.Date(paste0("2021-",format(tmp2$Date, "%m-%d"))), tmp2$Alt.1b, 
       type="l", col="red")
points(as.Date(paste0("2021-",format(tmp3$Date, "%m-%d"))), tmp3$Alt.1b, 
       type="l", col="blue")
legend("topleft", c("1975", "1976", "1997"), col=c("black","red", "blue"), 
       bty="n", lty=rep(1,3))
#1975: 7/16 for 1, 1a, 2, 2a & 7/12 for 1b, 2b, NoAct
#1976: 6/24 for 1, 1b, 2, 2b, NoAct
#1997: 7/5 for 1, 1a, 2, 2b, NoAct & 7/12 for 1b, 2a