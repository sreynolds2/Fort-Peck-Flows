
# EIS TABLES
fullrun_EIS<- FALSE
source("./R/3_load-and-clean_EIS.r")

## RETENTION TABLE LIKE CRAIGS
dat<- subset(EIS_full, anoxic_layer==FALSE)
tmp<- dcast(dat, Alt+Year+Hatch_Date~Develop_Mod+Drift_Mod, mean, 
            value.var="Retention")
write.csv(tmp, "./output/Retention_Comparison_EIS.csv",
          row.names = FALSE)


## STANDARD TABLES
tmp<- read.csv("./output/long-term_lambda_Standard_by_Year_EIS_no_zeros.csv",
               stringsAsFactors = FALSE)
tmp$Standard<- as.Date(tmp$Standard)
tmp$Hatch_Date<- as.Date(tmp$Hatch_Date)

### RETENTION TABLE
tbl2<- dcast(tmp, Year+Weather_Pattern~Flow_Scenario, value.var ="Retention")
tbl2[is.na(tbl2)]<- ""
tbl2<- tbl2[,c(1,3:9,2)]
write.csv(tbl2, "./output/Standard_Retentions_EIS_table.csv")

### LONG-TERM GROWTH RATE TABLE
tbl3<- dcast(tmp, Year+Weather_Pattern~Flow_Scenario, 
             value.var ="Long_Term_Growth_Rate")
tbl3[is.na(tbl3)]<- 0
tbl3<- tbl3[,c(1,3:9,2)]
write.csv(tbl3, "./output/Standard_LTPGR_EIS_table.csv")

### LONG-TERM GROWTH RATE TABLE LIKE GRAHAM'S
tbl_LTL<- expand.grid(Year=1930:2012, Flow_Scenario=unique(EIS_full$Alt))
tbl_LTL<- merge(tbl_LTL, tmp, by=c("Year", "Flow_Scenario"), all=TRUE)
tbl_LTL[which(is.na(tbl_LTL$Long_Term_Growth_Rate)),"Long_Term_Growth_Rate"]<- 0
tbl_LTL<- dcast(tbl_LTL, 
                Year~Flow_Scenario, 
                value.var = "Long_Term_Growth_Rate")

tbl_LTL<-tbl_LTL[,c(1,8,2:7)]
write.csv(tbl_LTL, 
          "./output/long-term_lambda_Standard_by_Year_EIS.csv", 
          row.names = FALSE)