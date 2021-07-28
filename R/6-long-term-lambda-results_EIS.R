setwd("./GitHub/Fort-Peck-Flows")

source("./R/0_default-parameters.r")
source("./R/1_global_DSM.r")
source("./R/2_functions.r")
fullrun_EIS<- FALSE
source("./R/3_load-and-clean_EIS.r")

any(EIS_full$Retention<=0)
  #IF TRUE, THEN NEED TO SUBSET THE DATA

EIS_full$Long_Term_Growth_Rate<- sapply(1:nrow(EIS_full), function(x)
{
  inps<- inputs
  inps$gamma<- 0.5
  inps$p_retained<- EIS_full$Retention[x]
  ea<- matrix_eigen_analysis(inps)
  return(ea$lambda1)
})

write.csv(EIS_full, "./output/long-term-lambda_EIS.csv",
          row.names = FALSE)

## RESULTS FOR STANDARD DATES AND NEW 0.9 DSM
EIS_full<- read.csv("./output/long-term-lambda_EIS.csv", 
                    stringsAsFactors = FALSE)
EIS_full[which(is.na(EIS_full$Alt)),]$Alt<- "NoAct"
EIS_full$Hatch_Date<- as.Date(EIS_full$Hatch_Date)

### USE ONLY THE NEW DEVELOPMENT MODEL WITH 0.9 DRIFT MODEL
dat<- subset(EIS_full, Develop_Mod=="New" & Drift_Mod==0.9 & anoxic_layer==FALSE)
dat$Spawn_Date<- dat$Hatch_Date-7

### USE ONLY STANDARD SPAWN DATES
spn<- read.csv("./output/Spawn_Dates_Summary_EIS.csv", stringsAsFactors = FALSE)
spn<- spn[,c("Year", "Weather_Pattern", "Flow_Scenario", "Standard")]
spn[which(spn$Flow_Scenario=="Alt1"),]$Flow_Scenario<- "Alt.1"
spn$Flow_Scenario<- gsub("Alt.", "", spn$Flow_Scenario)
spn[which(spn$Flow_Scenario=="No.Act"),]$Flow_Scenario<- "NoAct"
spn<- subset(spn, Standard!="Fail")
spn$Standard<- as.Date(paste(spn$Year, spn$Standard, sep="-"), format="%Y-%d-%b")

spn$Date<- as.character(format(spn$Standard, "%d-%b"))
tbl<- dcast(spn, Year+Weather_Pattern~Flow_Scenario, value.var ="Date")
tbl[is.na(tbl)]<- ""
tbl<- tbl[,c(1,3:9,2)]
write.csv(tbl, "./output/Standard_Dates_EIS_table.csv")

tmp<- merge(spn, dat, by.x=c("Year", "Flow_Scenario", "Standard"), 
            by.y=c("Year", "Alt", "Spawn_Date"), all.x=TRUE)
tmp<- tmp[-which(duplicated(tmp[,c("Year", "Flow_Scenario", "Standard")], 
                            fromLast = TRUE)),]
write.csv(tmp, "./output/long-term_lambda_Standard_by_Year_EIS_no_zeros.csv",
          row.names = FALSE) 

