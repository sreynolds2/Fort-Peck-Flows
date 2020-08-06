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

