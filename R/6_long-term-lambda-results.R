setwd("./GitHub/Fort-Peck-Flows")

source("./R/1_global.r")
source("./R/2_functions.r")
full_run<- FALSE
source("./R/3_load-and-clean.r")

tmp<- subset(dat, Retention_Probability>0)

tmp$Long_Term_Growth_Rate<- sapply(1:nrow(tmp), function(x)
{
  inps<- inputs
  inps$gamma<- 0.5
  inps$p_retained<- tmp$Retention_Probability[x]
  ea<- matrix_eigen_analysis(inps)
  return(ea$lambda1)
})

dat_LTL<- merge(dat, tmp, all=TRUE)
## FIX 1930 ALT 1 ISSUE
dat_LTL[which(dat_LTL$Year==1930 & dat_LTL$Flow_Scenario=="Alt1"),
        c("Retention_Probability", "Long_Term_Growth_Rate")]<- c(0,NA)
#dat_LTL[which(dat_LTL$Year==1930 & dat_LTL$Flow_Scenario=="Alt1"),
#        c("Retention_Probability", "Long_Term_Growth_Rate")]<- c(0,0)
#dat_LTL[which(is.na(dat_LTL$Long_Term_Growth_Rate)),"Long_Term_Growth_Rate"]<- 0
write.csv(dat_LTL, "./output/long-term_lambda_by_flow_and_year.csv",row.names = FALSE)


# TABLE LIKE GRAHAM'S
tmp<- expand.grid(Year=1930:2012, Flow_Scenario=unique(dat$Flow_Scenario))
tbl_LTL<- merge(dat_LTL, tmp, all=TRUE)
tbl_LTL[which(is.na(tbl_LTL$Long_Term_Growth_Rate)),"Long_Term_Growth_Rate"]<- 0
#tbl_LTL[which(is.na(tbl_LTL$Long_Term_Growth_Rate)),"Long_Term_Growth_Rate"]<- ""
tbl_LTL<- dcast(tbl_LTL, Year~Flow_Scenario, value.var = "Long_Term_Growth_Rate")
tbl_LTL<-tbl_LTL[,c("Year", "AltNA", "Alt1", "Alt1a","Alt1b", "Alt2", "Alt2a","Alt2b")]
write.csv(tbl_LTL, "./output/long-term_lambda_by_flow_and_year_version2.csv", row.names = FALSE)


# ANALYZE TABLE WITH ZEROS INCLUDED
tbl<- read.csv( "./output/long-term_lambda_by_flow_and_year_version2.csv")
## ARITHMETIC MEAN
arith_mean_Zeros<- colMeans(tbl[,2:ncol(tbl)])
## MIN, MAX, AND MEDIAN BY FLOW FAMILY
max0<- sapply(2:ncol(tbl), function(j)
{
  max(tbl[,j])  
})
min0<- sapply(2:ncol(tbl), function(j)
{
  min(tbl[,j])  
})
med0<- sapply(2:ncol(tbl), function(j)
{
  median(tbl[,j])  
})

# ANALYZE SUCCESSES ONLY
tbl[tbl==0]<- NA
## NUMBER OF YEARS WITH RETENTION BY ALTERNATIVE
counts<- sapply(2:ncol(tbl), function(j)
{
  length(which(!is.na(tbl[,j])))  
})

## GEOMETRIC MEAN ACROSS SUCCESSFUL YEARS BY ALTERNATIVE
geom_mean<- sapply(2:ncol(tbl), function(j)
{
  prod(tbl[,j], na.rm = TRUE)^(1/counts[j-1]) 
})

## MIN, MAX, AND MEDIAN
maxS<- sapply(2:ncol(tbl), function(j)
{
  max(tbl[,j], na.rm = TRUE)  
})
minS<- sapply(2:ncol(tbl), function(j)
{
  min(tbl[,j], na.rm = TRUE)  
})
medS<- sapply(2:ncol(tbl), function(j)
{
  median(tbl[,j], na.rm = TRUE)  
})

# FLOW SCENARIO TABLE SUMMARY
tbl_sum<- rbind(arith_mean_Zeros, min0, med0, max0, counts,geom_mean, minS, medS, maxS)
rownames(tbl_sum)<- c("Arithmetic_Mean_Full", "Min_Full", "Median_Full",
                       "Max_Full", "Number_of_Successes", "Geometric_Mean_Successes",
                       "Min_Successes", "Median_Successes","Max_Successes") 
write.csv(tbl_sum, "./output/summary_by_flow_and_year_version2.csv")

# COMBINING ALTERNATIVE AND VARIANTS BY MAX OUTCOME
dat<- read.csv("./output/long-term_lambda_by_flow_and_year.csv")
## ADD IN MISSING YEARS
tmp<- expand.grid(Year=1930:2012, Flow_Scenario=unique(dat$Flow_Scenario))
dat<- merge(dat, tmp, all=TRUE)
## CREATE FLOW FAMILY INDICATOR
dat$Flow_Family<- "NA" 
dat[grep(1, dat$Flow_Scenario), "Flow_Family"]<- 1
dat[grep(2, dat$Flow_Scenario), "Flow_Family"]<- 2
## CHANGE NA's TO 0's
dat[which(is.na(dat$Long_Term_Growth_Rate)),"Long_Term_Growth_Rate"]<- 0
## GENERATE TABLE
tbl2<- dcast(dat, Year~Flow_Family, fun.aggregate=max, value.var = "Long_Term_Growth_Rate", fill=0)
tbl2<- tbl2[,c("Year", "NA", "1", "2")]
write.csv(tbl2, "./output/max_long-term_lambda_by_flow_family_and_year_version2.csv", row.names = FALSE)

# FLOW FAMILY ANALYSES
## ARITHMETIC MEAN
arith_mean_Zeros2<- colMeans(tbl2[,2:ncol(tbl2)])
## MIN, MAX, AND MEDIAN
max02<- sapply(2:ncol(tbl2), function(j)
{
  max(tbl2[,j])  
})
min02<- sapply(2:ncol(tbl2), function(j)
{
  min(tbl2[,j])  
})
med02<- sapply(2:ncol(tbl2), function(j)
{
  median(tbl2[,j])  
})

## ANALYZE SUCCESSES ONLY
tbl2[tbl2==0]<- NA
### NUMBER OF YEARS WITH RETENTION
counts2<- sapply(2:ncol(tbl2), function(j)
{
  length(which(!is.na(tbl2[,j])))  
})

### GEOMETRIC MEAN ACROSS SUCCESSFUL YEARS
geom_mean2<- sapply(2:ncol(tbl2), function(j)
{
  prod(tbl2[,j], na.rm = TRUE)^(1/counts2[j-1]) 
})

### MIN, MAX, AND MEDIAN BY FLOW FAMILY
maxS2<- sapply(2:ncol(tbl2), function(j)
{
  max(tbl2[,j], na.rm = TRUE)  
})
minS2<- sapply(2:ncol(tbl2), function(j)
{
  min(tbl2[,j], na.rm = TRUE)  
})
medS2<- sapply(2:ncol(tbl2), function(j)
{
  median(tbl2[,j], na.rm = TRUE)  
})

# FLOW FAMILY TABLE SUMMARY
tbl2_sum<- rbind(arith_mean_Zeros2, min02, med02, max02, counts2,geom_mean2, minS2, medS2, maxS2)
rownames(tbl2_sum)<- c("Arithmetic_Mean_Full", "Min_Full", "Median_Full",
                       "Max_Full", "Number_of_Successes", "Geometric_Mean_Successes",
                       "Min_Successes", "Median_Successes","Max_Successes") 
write.csv(tbl2_sum, "./output/summary_by_flow_family_version2.csv")

