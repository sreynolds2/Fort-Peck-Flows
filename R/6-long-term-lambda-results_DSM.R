setwd("./GitHub/Fort-Peck-Flows")

source("./R/0_default-parameters.r")
source("./R/1_global_DSM.r")
source("./R/2_functions.r")
fullrun_DSM<- FALSE
source("./R/3_load-and-clean_DSM.r")

any(DSM_full$Retention<=0)
  #IF TRUE, THEN NEED TO SUBSET THE DATA
DSM_full$Long_Term_Growth_Rate<- sapply(1:nrow(DSM_full), function(x)
{
  inps<- inputs
  inps$gamma<- 0.5
  inps$p_retained<- DSM_full$Retention[x]
  ea<- matrix_eigen_analysis(inps)
  return(ea$lambda1)
})

write.csv(DSM_full, "./output/long-term-lambda_DSM.csv",
          row.names = FALSE)

# TABLE LIKE GRAHAM'S
DSM_full<- read.csv("./output/long-term-lambda_DSM.csv", 
                    stringsAsFactors = FALSE)
DSM_full[which(is.na(DSM_full$Alt)),]$Alt<- "NoAct"
DSM_full$Hatch_Date<- as.Date(DSM_full$Hatch_Date)

# USE ONLY THE NEW DEVELOPMENT MODEL WITH 0.9 DRIFT MODEL
dat<- subset(DSM_full, Develop_Mod=="New" & Drift_Mod==0.9 & anoxic_layer==FALSE)
dat$Spawn_Date<- dat$Hatch_Date-7

# USE ONLY STANDARD SPAWN DATES
spn<- read.csv("./output/Spawn_Dates_Summary.csv", stringsAsFactors = FALSE)
spn<- spn[,c("Year", "Weather_Pattern", "Flow_Scenario", "Standard")]
spn[which(spn$Flow_Scenario=="Alt1"),]$Flow_Scenario<- "Alt.1"
spn$Flow_Scenario<- gsub("Alt.", "", spn$Flow_Scenario)
spn[which(spn$Flow_Scenario=="No.Act"),]$Flow_Scenario<- "NoAct"
spn<- subset(spn, Standard!="Fail")
spn$Standard<- as.Date(paste(spn$Year, spn$Standard, sep="-"))
spn[which(spn$Year==1975),]$Standard<- "1975-07-02" #NEEDS CHANGING EVENTUALLY
spn[which(spn$Year==1976),]$Standard<- "1976-06-14" #NEEDS CHANGING EVENTUALLY
spn[which(spn$Year==1997),]$Standard<- "1997-06-25" #NEEDS CHANGING EVENTUALLY
spn[which(spn$Year==1997 & spn$Flow_Scenario %in% c("1b", "2a")),]$Standard<- "1997-06-30" #NEEDS CHANGING EVENTUALLY

tmp<- merge(spn, dat, by.x=c("Year", "Flow_Scenario", "Standard"), 
            by.y=c("Year", "Alt", "Spawn_Date"), all.x=TRUE)
tmp[which(tmp$Year==1997 & tmp$Flow_Scenario=="1b"), 5:10]<- #APPROXIMATE BASED ON SIMILAR FLOWS AND SAME HATCH DATE
  tmp[which(tmp$Year==1997 & tmp$Flow_Scenario=="2a"),5:10] #NEEDS CHANGING EVENTUALLY
write.csv(tmp, "./output/long-term_lambda_Standard_by_Year_DSM_no_zeros.csv",
          row.names = FALSE)

tbl_LTL<- expand.grid(Year=1930:2012, Flow_Scenario=unique(DSM_full$Alt))
tbl_LTL<- merge(tbl_LTL, tmp, by=c("Year", "Flow_Scenario"), all=TRUE)
tbl_LTL[which(is.na(tbl_LTL$Long_Term_Growth_Rate)),"Long_Term_Growth_Rate"]<- 0
tbl_LTL<- dcast(tbl_LTL, 
                Year~Flow_Scenario, 
                value.var = "Long_Term_Growth_Rate")
  
tbl_LTL<-tbl_LTL[,c(1,8,2:7)]
write.csv(tbl_LTL, 
          "./output/long-term_lambda_Standard_by_Year_DSM.csv", 
          row.names = FALSE)


## PLOT OF OUTCOMES WITH CHANGES IN AGE-0 SURVIVAL
### PULL 1985 STANDARD DATA
tmp<- tmp[which(tmp$Year==1985),]
LTLs<- lapply(1:nrow(tmp), function(x)
{
  inps<- inputs
  inps$p_retained<- tmp$Retention[x]
  inps$gamma<- 0.5
  out<- lapply(seq(0.00002, 0.002, 0.00002), function(y)
  {
    inps$phi0_MR<- y
    ea<- matrix_eigen_analysis(inps)
    outt<- data.frame(Flow_Scenario=tmp$Flow_Scenario[x],
                      Spawn_Date=tmp$Standard[x],
                      Retention=inps$p_retained,
                      phi0_MR=inps$phi0_MR,
                      Long_Term_Growth_Rate=ea$lambda1)
    return(outt)
  })
  out<- do.call(rbind, out)
})
LTLs<- do.call(rbind, LTLs)

par(mfrow=c(1,1))
alts<- unique(tmp$Flow_Scenario)
cls<- c("darkgreen", "blue", "black", "red")
plot(LTLs[which(LTLs$Flow_Scenario==alts[1]),]$phi0_MR,
     LTLs[which(LTLs$Flow_Scenario==alts[1]),]$Long_Term_Growth_Rate,
     ylim=c(0.75, 1.1), xlab="Age-0 Survival Given Retention", 
     ylab="Long-Term Population Growth Rate", type="l", lwd=2, 
     col=cls[1], tck=0.02, mgp=c(1.5,0.1,0))
invisible(lapply(2:length(alts), function(i)
{
  points(LTLs[which(LTLs$Flow_Scenario==alts[i]),]$phi0_MR,
       LTLs[which(LTLs$Flow_Scenario==alts[i]),]$Long_Term_Growth_Rate,
       ylim=c(0.75, 1), xlab="Age-0 Survival Given Retention", 
       ylab="Long-Term Population Growth Rate", type="l", lwd=2,
       col=cls[i]) 
}))
legend("bottomright", paste0("Alternative ", alts), lwd=2, col=cls, 
       bty="n")

# FULL TABLE (ALL DRIFT AND DEVELOPMENT MODELS) LIKE GRAHAM'S
DSM_full<- read.csv("./output/long-term-lambda_DSM.csv", 
                    stringsAsFactors = FALSE)
DSM_full[which(is.na(DSM_full$Alt)),]$Alt<- "NoAct"
DSM_full$Hatch_Date<- as.Date(DSM_full$Hatch_Date)

tmp<- expand.grid(Year=1930:2012, Flow_Scenario=unique(DSM_full$Alt))
tbl_LTL<- merge(DSM_full, tmp, by.x=c("Year", "Alt"), 
                by.y=c("Year", "Flow_Scenario"), all=TRUE)
tbl_LTL[which(is.na(tbl_LTL$Long_Term_Growth_Rate)),"Long_Term_Growth_Rate"]<- 0
tbl_LTL<- dcast(tbl_LTL, 
                Year+Hatch_Date+Develop_Mod+Drift_Mod+anoxic_layer~Alt, 
                mean, value.var = "Long_Term_Growth_Rate")
  ## SEE NOTES ON REPEATS AND UPDATE WHEN RESOLVED
tbl_LTL[which(is.na(tbl_LTL$`1`)),]$`1`<-0
tbl_LTL[which(is.na(tbl_LTL$`1a`)),]$`1a`<-0
tbl_LTL[which(is.na(tbl_LTL$`1b`)),]$`1b`<-0
tbl_LTL[which(is.na(tbl_LTL$`2`)),]$`2`<-0
tbl_LTL[which(is.na(tbl_LTL$`2a`)),]$`2a`<-0
tbl_LTL[which(is.na(tbl_LTL$`2b`)),]$`2b`<-0
tbl_LTL[which(is.na(tbl_LTL$`NA`)),]$`NA`<-0

tbl_LTL<-tbl_LTL[,c(1:5,12,6:11)]
write.csv(tbl_LTL, 
          "./output/long-term_lambda_by_flow_and_hatch_date_DSM.csv", 
          row.names = FALSE)

key_LTL<- subset(tbl_LTL, Develop_Mod=="New" & Drift_Mod==0.9 & 
                   anoxic_layer==FALSE)
tmp<- data.frame(Year=1930:2012, Develop_Mod="New", Drift_Mod=0.9,
                 anoxic_layer=FALSE)
key_LTL<- merge(key_LTL, tmp, all=TRUE)
key_LTL[which(is.na(key_LTL$`1`)),]$`1`<-0
key_LTL[which(is.na(key_LTL$`1a`)),]$`1a`<-0
key_LTL[which(is.na(key_LTL$`1b`)),]$`1b`<-0
key_LTL[which(is.na(key_LTL$`2`)),]$`2`<-0
key_LTL[which(is.na(key_LTL$`2a`)),]$`2a`<-0
key_LTL[which(is.na(key_LTL$`2b`)),]$`2b`<-0
key_LTL[which(is.na(key_LTL$`NA`)),]$`NA`<-0

