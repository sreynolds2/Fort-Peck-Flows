
# R1
tbl_LTL<- read.csv("./output/long-term_lambda_Standard_by_Year_DSM_new.csv", 
                   stringsAsFactors = FALSE)
names(tbl_LTL)<- gsub("X", "", names(tbl_LTL))
R1<- tbl_LTL[which(tbl_LTL$Year %in% 1983:1995),]
write.csv(R1, "./output/Table_R1.csv", row.names = FALSE)


# R2
source("./R/0_default-parameters.r")
source("./R/2_functions.r")
tmp<- read.csv("./output/long-term_lambda_Standard_by_Year_DSM_no_zeros_new.csv",
               stringsAsFactors = FALSE)
pret<- mean(tmp$Retention)
sp<- 0.5
inps<- inputs
inps$p_retained<- pret
inps$gamma<- sp
bl<-matrix_eigen_analysis(inps)
bl$lambda1

tbl<-sens_elas_table(data=bl, number=11)
s<- tbl$Sensitivities[2:11,]
par(mfrow=c(1,1))
barplot(s$Sensitivity, names.arg = s$Parameter, space=0, horiz=TRUE)

e<- tbl$Elasticities
