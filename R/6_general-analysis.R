source("./R/1_global.r")
source("./R/2_functions.r")
full_run<- FALSE
source("./R/3_load-and-clean.r")


###################################################
###################################################
###################################################

## OLD INPUTS
# inps<- inputs
# inps$gamma<- 0.25
# inps$p_retained<-0.1
# bl2<-matrix_eigen_analysis(inps)
# tbl2<- sens_elas_table(data=bl2)

## RESULTS SECTION 1
bl<-matrix_eigen_analysis(inputs)
bl$lambda1

tbl<-sens_elas_table(data=bl, number=56)
tbl$Sensitivities[1:7,]
tmp<-tbl$Sensitivities[which(tbl$Sensitivities$Type!="Entry"),]
tmp[1:6,]
write.csv(tmp[2:6, 1:2], "./output/Baseline_Top_5_Sens.csv", 
          row.names = FALSE)
tbl$Elasticities[1:22,]
write.csv(tbl$Elasticities[1:21,1:2], "./output/Baseline_Top_Elas.csv", 
          row.names = FALSE)
tbl$Elasticities$vals<- sapply(1:nrow(tbl$Elasticities), function(i)
{
  if(tbl$Elasticities$Parameter[i] %in% names(bl$inputs))
  {
    out<-bl$inputs[[which(names(bl$inputs)==tbl$Elasticities$Parameter[i])]]
  }
  if(!(tbl$Elasticities$Parameter[i] %in% names(bl$inputs)))
  {
    if(tbl$Elasticities$Parameter[i]=="sexratio")
    {
      out<-bl$inputs$probF
    }
    if(tbl$Elasticities$Parameter[i]=="prod")
    {
      out<-bl$inputs$gamma*bl$inputs$phi0_MR*bl$inputs$p_retained
    }
    if(grepl("phi", tbl$Elasticities$Parameter[i]))
    {
      indx<-as.numeric(strsplit(tbl$Elasticities$Parameter[i],"phi")[[1]][2])
      out<- bl$inputs$phi[indx]
    }
  }
  return(out)
})
tbl$Elasticities$Potential<- tbl$Elasticities$Elasticity*(1-tbl$Elasticities$vals)/tbl$Elasticities$vals
tbl$Elasticities<- tbl$Elasticities[order(abs(tbl$Elasticities$Potential), decreasing = TRUE),]
tbl$Elasticities[1:22,]
write.csv(tbl$Elasticities[1:21,c(1:2,6)], "./output/Baseline_Top_Elas_w_Potential.csv", 
          row.names = FALSE)

inps<- inputs
inps$gamma<- 0.5
inps$p_retained<-0.25
inps$phi0_MR<-0.0004
hope<- matrix_eigen_analysis(inps)
hope$lambda1
tblH<- sens_elas_table(data=hope)
tblH$Sensitivities
tblH$Elasticities

#FIGURE 7
bnd_inp<- boundary_product(inputs)
crvs<- spawning_survival_retention_curves(boundary_inputs = bnd_inp)
plot_boundary_curves(crvs, phi0_upper = 0.01, xlab="", ylab="")
legend(0, 0.002, expression(paste(p[ret],"=1")), bty='n')
legend(0.35, 0.007, expression(paste(p[ret],"=0.1")), bty='n')
mtext(expression(paste("Spawning Probability (", gamma, ")")), 1,
      outer=TRUE, padj=1.2)
mtext(expression(paste("Age-0 Survival ( ", phi[0], ")")), 2,
      outer=TRUE, padj=2, las=0)

#FIGURE 8
par(mfrow=c(2,1),
    oma = c(3,-0.1,1,0) + 0.1,
    mar = c(0.2,4,1,1) + 0.1,
    las=1)
##TOP
crvs<- spawning_survival_retention_curves(boundary_inputs = bnd_inp, 
                                          p_retained = 0.2)
plot_boundary_curves(crvs, phi0_upper = 0.005, xaxis="n", xlabel="", ylabel="")
legend(0.05, 0.005, expression(paste(p[ret],"=0.2")), bty='n')
legend("topright", "A", bty='n')
points(0.5, 0.000075, pch=16, col="red")
points(0.5, 0.0025, pch=16, col="blue")
abline(0.0004, 0, lty="dashed")
##BOTTOM
crvs<- spawning_survival_retention_curves(boundary_inputs = bnd_inp, 
                                          p_retained = 0.7)
plot_boundary_curves(crvs, phi0_upper = 0.005, xlabel="", ylabel="")
legend(0.03, 0.005, expression(paste(p[ret],"=0.7")), bty='n')
legend("topright", "B", bty='n')
points(0.5, 0.000075, pch=16, col="red")
points(0.5, 0.0025, pch=16, col="blue")
abline(0.0004, 0, lty="dashed")
mtext(expression(paste("Spawning Probability  (", gamma, ")")), 1, padj=1.5)
mtext(expression(paste("Age-0 Survival  (  ", phi[0], ")")), 2, 
      outer=TRUE, las=0, padj=2)

crvs<- spawning_survival_retention_curves(boundary_inputs = bnd_inp,
                                          p_retained = dat$p_retained)
plot_boundary_curves(crvs, phi0_upper = 0.05)

###################################################
###################################################
###################################################

## ONLY RUN THE FOLLOWING IF NEED TO INCORPORATE NEW DATA!!!
# dat$lambda<- sapply(1:nrow(dat), function(i)
# {
#   inps<- inputs
#   inps$p_retained<- dat$p_retained[i]
#   ea<- matrix_eigen_analysis(inps)
#   ea$alternative<- dat$scenario[i]
#   ea$year<- dat$year[i]
#   ea$temp_flow<- dat$temperature_flow[i]
#   ea$id<- dat$id[i]
#   saveRDS(ea, paste0("./output/scenario_eigen_analysis_", dat$id[i], ".rds"))
#   return(ea$lambda1)
# })
# dat$lambda_25<- sapply(1:nrow(dat), function(i)
# {
#   inps<- inputs
#   inps$p_retained<- dat$p_retained[i]
#   inps$gamma<- 0.25
#   ea<- matrix_eigen_analysis(inps)
#   ea$alternative<- dat$scenario[i]
#   ea$year<- dat$year[i]
#   ea$temp_flow<- dat$temperature_flow[i]
#   ea$id<- dat$id[i]
#   saveRDS(ea, paste0("./output/scenario_eigen_analysis_g25_", dat$id[i], ".rds"))
#   return(ea$lambda1)
# })
# dat$lambda_50<- sapply(1:nrow(dat), function(i)
# {
#   inps<- inputs
#   inps$p_retained<- dat$p_retained[i]
#   inps$gamma<- 0.5
#   ea<- matrix_eigen_analysis(inps)
#   ea$alternative<- dat$scenario[i]
#   ea$year<- dat$year[i]
#   ea$temp_flow<- dat$temperature_flow[i]
#   ea$id<- dat$id[i]
#   saveRDS(ea, paste0("./output/scenario_eigen_analysis_g50_", dat$id[i], ".rds"))
#   return(ea$lambda1)
# })
# dat$lambda_75<- sapply(1:nrow(dat), function(i)
# {
#   inps<- inputs
#   inps$p_retained<- dat$p_retained[i]
#   inps$gamma<- 0.75
#   ea<- matrix_eigen_analysis(inps)
#   ea$alternative<- dat$scenario[i]
#   ea$year<- dat$year[i]
#   ea$temp_flow<- dat$temperature_flow[i]
#   ea$id<- dat$id[i]
#   saveRDS(ea, paste0("./output/scenario_eigen_analysis_g75_", dat$id[i], ".rds"))
#   return(ea$lambda1)
# })
# dat$lambda_50_sr50<- sapply(1:nrow(dat), function(i)
# {
#   inps<- inputs
#   inps$p_retained<- dat$p_retained[i]
#   inps$gamma<- 0.5
#   inps$probF<- 0.5
#   ea<- matrix_eigen_analysis(inps)
#   ea$alternative<- dat$scenario[i]
#   ea$year<- dat$year[i]
#   ea$temp_flow<- dat$temperature_flow[i]
#   ea$id<- dat$id[i]
#   saveRDS(ea, paste0("./output/scenario_eigen_analysis_g50_sr50_", dat$id[i], ".rds"))
#   return(ea$lambda1)
# })
# dat$year<- ifelse(dat$year<20, as.numeric(paste0("20", dat$year)),
#                   as.numeric(paste0("19", dat$year)))
# write.csv(dat, "./output/baseline_scenario_lambda_data.csv", row.names = FALSE)

###################################################
###################################################
###################################################


dat<- read.csv("./output/baseline_scenario_lambda_data.csv")


###################################################
###################################################
###################################################

## MEDIAN TEMPERATURES
dat[dat$scenario=="Alt2a" & dat$year==1987 & dat$temperature_flow=="M2",]$temperature_flow<- "M"
dat[dat$scenario=="Alt2a" & dat$year==1966 & dat$temperature_flow=="M5",]$temperature_flow<- "M"
lambdaM<- dcast(dat[which(dat$temperature_flow=="M" & is.na(dat$spawn_date)),], 
                year~scenario, max, value.var="lambda_50", fill=0)
# CREATE TABLE 3
#lambdaM[lambdaM==0]<-""
#write.csv(lambdaM, "./output/lambda_by_alt_and_year_temp_M_g50.csv", row.names = FALSE)


## LOW TEMPERATURES
dat[dat$scenario=="Alt2a" & dat$year==1966 & dat$temperature_flow=="L8",]$temperature_flow<- "L"
dat[dat$scenario=="Alt2a" & dat$year==1987 & dat$temperature_flow=="L2",]$temperature_flow<- "L"
lambdaL<- dcast(dat[which(dat$temperature_flow=="L" & is.na(dat$spawn_date)),], 
                year~scenario, max, value.var="lambda_50", fill=0)
lambdaLdiff<-lambdaL
lambdaLdiff[,2:8]<- lambdaL[,2:8]-lambdaM[,2:8]
# CALCULATE MEAN VALUES
lambdaLdiff[lambdaLdiff==0]<-NA
lambdaLdiff$Alt2b[which(lambdaLdiff$year==1984)]<-NA
colMeans(lambdaLdiff[,2:8], na.rm=TRUE)
mean(unname(unlist(lambdaLdiff[,2:8])), na.rm = TRUE)
median(unname(unlist(lambdaLdiff[,2:8])), na.rm = TRUE)
# CREATE FOR TABLE 4
# lambdaLdiff[lambdaLdiff==0]<-""
# write.csv(lambdaLdiff, "./output/lambda_by_alt_and_year_temp_Ldiff_g50.csv", row.names = FALSE)
# CREATE TABLE SIMILAR TO TABLE 3 FOR LOW TEMPERATURE
# lambdaL[lambdaL==0]<-""
# write.csv(lambdaL, "./output/lambda_by_alt_and_year_temp_L_g50.csv", row.names = FALSE)
lambdaL[lambdaL==0]<-NA
lambdaL$scenario_max<- sapply(1:nrow(lambdaL), function(x)
{
  names(which.max(lambdaL[x,2:ncol(lambdaL)]))
})
lambdaL$max_diff<- sapply(1:nrow(lambdaL), function(x)
{
  diff<-max(lambdaL[x,2:(ncol(lambdaL)-1)], na.rm=TRUE)-min(lambdaL[x,2:(ncol(lambdaL)-1)], na.rm=TRUE)
  return(diff)
})

## HIGH TEMPERATURES
dat[dat$scenario=="Alt2a" & dat$year==1987 & dat$temperature_flow=="H2",]$temperature_flow<- "H"
dat[dat$scenario=="Alt2a" & dat$year==1966 & dat$temperature_flow=="H2",]$temperature_flow<- "H"
lambdaH<- dcast(dat[which(dat$temperature_flow=="H" & is.na(dat$spawn_date)),], 
                year~scenario, max, value.var="lambda_50", fill=0)
lambdaHdiff<-lambdaH
lambdaHdiff[,2:8]<- lambdaH[,2:8]-lambdaM[,2:8]
# CALCULATE MEAN VALUES
lambdaHdiff[lambdaHdiff==0]<-NA
lambdaHdiff$Alt2b[which(lambdaHdiff$year==1984)]<-NA
hist(colMeans(lambdaHdiff[,2:8], na.rm=TRUE))
mean(unname(unlist(lambdaHdiff[,2:8])), na.rm = TRUE)
median(unname(unlist(lambdaHdiff[,2:8])), na.rm = TRUE)
# CREATE FOR TABLE 4
#lambdaHdiff[lambdaHdiff==0]<-""
#write.csv(lambdaHdiff, "./output/lambda_by_alt_and_year_temp_Hdiff_g50.csv", row.names = FALSE)
# CREATE TABLE SIMILAR TO TABLE 3 FOR HIGH TEMPERATURE
#lambdaH[lambdaH==0]<-""
#write.csv(lambdaH, "./output/lambda_by_alt_and_year_temp_H_g50.csv", row.names = FALSE)
lambdaH[lambdaH==0]<-NA
lambdaH$scenario_max<- sapply(1:nrow(lambdaH), function(x)
{
  names(which.max(lambdaH[x,2:ncol(lambdaH)]))
})
lambdaH$max_diff<- sapply(1:nrow(lambdaH), function(x)
{
  diff<-max(lambdaH[x,2:(ncol(lambdaH)-1)], na.rm=TRUE)-min(lambdaH[x,2:(ncol(lambdaH)-1)], na.rm=TRUE)
  return(diff)
})

#CREATE TABLE 5
lambdaM[lambdaM==0]<-NA
lambdaM$scenario_max<- sapply(1:nrow(lambdaM), function(x)
{
  names(which.max(lambdaM[x,2:ncol(lambdaM)]))
})
lambdaM$max_diff<- sapply(1:nrow(lambdaM), function(x)
{
  diff<-max(lambdaM[x,2:(ncol(lambdaM)-1)], na.rm=TRUE)-min(lambdaM[x,2:(ncol(lambdaM)-1)], na.rm=TRUE)
  return(diff)
})
tbl5<- data.frame(Year=lambdaM$year,
                  Low=lambdaL$scenario_max,
                  Median=lambdaM$scenario_max,
                  High=lambdaH$scenario_max)
#write.csv(tbl5, "./output/top_scenarios_by_year_g50.csv", row.names = FALSE)

###################################################
###################################################
###################################################

## YEAR 85 ROBUSTNESS ANALYSIS
flows<- dcast(dat[which(dat$year==1985),], scenario~temperature_flow, 
              max, value.var="lambda_50", fill=0)
flows[flows==0]<-NA
flows<- flows[,c("scenario", "LF", "MF", "HF", "L", "M", "H", "LP", "MP", "HP")]
flows<- flows[,c("scenario", "LF", "L", "LP", "MF", "M", "MP", "HF", "H", "HP")]

# CREATE FIGURE 9
par(mfrow=c(3,2),
    oma = c(3,2,1,0) + 0.1,
    mar = c(0.2,3,1,1) + 0.1,
    las=1)
plot(unname(unlist(flows[1,2:10])), xaxt="n", xlab="", ylab="", tck=0.02, mgp=c(1.5,0.1,0))
axis(1, at=1:9, tck=0.02, mgp=c(1.5,0.1,0), labels=rep("",9))
legend("topleft", "Alt. 1", bty="n")
plot(unname(unlist(flows[4,2:10])), xaxt="n", xlab="", ylab="", tck=0.02, mgp=c(1.5,0.1,0))
axis(1, at=1:9, tck=0.02, mgp=c(1.5,0.1,0), labels=rep("",9))
legend("topleft", "Alt. 2", bty="n")
plot(unname(unlist(flows[2,2:10])), xaxt="n", xlab="", ylab="", tck=0.02, mgp=c(1.5,0.1,0))
axis(1, at=1:9, tck=0.02, mgp=c(1.5,0.1,0), labels=rep("",9))
legend("topleft", "Alt. 1a", bty="n")
plot(unname(unlist(flows[5,2:10])), xaxt="n", xlab="", ylab="", tck=0.02, mgp=c(1.5,0.1,0))
axis(1, at=1:9, tck=0.02, mgp=c(1.5,0.1,0), labels=rep("",9))
legend("topleft", "Alt. 2a", bty="n")
plot(unname(unlist(flows[3,2:10])), xaxt="n", xlab="", ylab="", tck=0.02, mgp=c(1.5,0.1,0))
axis(1, at=1:9, tck=0.02, mgp=c(1.5,0.1,0), 
     labels=c("LF", "L", "LP", "MF", "M", "MP", "HF", "H", "HP"))
legend("topleft", "Alt. 1b", bty="n")
plot(unname(unlist(flows[6,2:10])), xaxt="n", xlab="", ylab="", tck=0.02, mgp=c(1.5,0.1,0))
axis(1, at=1:9, tck=0.02, mgp=c(1.5,0.1,0), 
     labels=c("LF", "L", "LP", "MF", "M", "MP", "HF", "H", "HP"))
legend("topleft", "Alt. 2b", bty="n")
mtext("Historical Temperature - Powerhouse Alternative Combination", 1,
      outer=TRUE, padj=1.5)
mtext("Long-Term Growth Rate", 2, outer=TRUE, las=0)


###################################################
###################################################
###################################################

#REMINDER:  CHANGE H2's, etc. if needed
# PRODUCE A VERSION OF TABLE 7
sc<-unique(dat$scenario)
avg_dat<-lapply(sc, function(x)
{
  tmp<-dat[which(dat$scenario==x & is.na(dat$spawn_date) & dat$temperature_flow=="M"),]
  y<-nrow(tmp)
  avg<-(prod(tmp$lambda_50))^(1/y)
  if(x=="Alt1")
  {
    min<-(avg^(y+1)*inputs$phi[1]^(83-y-1))^(1/83)
    max<-(avg^(y+1)*inputs$phi[15]^(83-y-1))^(1/83)
  }
  if(x!="Alt1")
  {
    min<-(prod(tmp$lambda_50)*inputs$phi[1]^(83-y))^(1/83)
    max<-(prod(tmp$lambda_50)*inputs$phi[15]^(83-y))^(1/83)
  }
  out<-data.frame(no_years=y, avg_lambda=avg, min_lambda=min, max_lambda=max)
  return(out)
})
avg_dat<-do.call("rbind", avg_dat)
# write.csv(avg_dat, "./output/average-and-POR-lambdas.csv", row.names=FALSE)


###################################################
###################################################
###################################################

# BOUNDARY CURVES BY SCENARIO
inps<-inputs
inps$gamma<-0.5
sc<- unique(dat$scenario)
par(mfrow=c(3,3),
    oma = c(3,2,1,0) + 0.1,
    mar = c(0.2,3,1,1) + 0.1,
    las=1)
invisible(lapply(sc, function(x)
{
  bnd_inp<- boundary_product(inps)
  tmp<- dat[which(dat$scenario==x & is.na(dat$spawn_date) & dat$temperature_flow=="M"),]
  crvs<- spawning_survival_retention_curves(boundary_inputs = bnd_inp, 
                                            p_retained = tmp$p_retained)
  plot_boundary_curves(crvs, phi0_upper = 1, xaxis="n", xlabel="", ylabel="")
  if(x!="Alt2a" & x!="Alt2b"& x!="NoAct")
  {
    axis(1, at=seq(0,0.7, 0.1), labels=rep("",8), 
         tck=0.02, mgp=c(1.5,0.1,0))
  }
  if(x=="Alt2a" | x=="Alt2b" | x=="NoAct")
  {
    axis(1, at=seq(0,0.7, 0.1), tck=0.02, mgp=c(1.5,0.1,0))
  }
  legend("topright", paste(x), bty="n")
}))
mtext(expression(paste("Spawning Probability (", gamma, ")")),
      1, outer=TRUE, padj=1)
mtext(expression(paste("Age-0 Survival ( ", phi[0], ")")),
      2, outer=TRUE, padj=1, las=0)


# BOUNDARY CURVES BY YEAR
inps<-inputs
inps$gamma<-0.5
yr<- unique(dat$year)
par(mfrow=c(4,4),
    oma = c(3,2,1,0) + 0.1,
    mar = c(0.2,3,1,1) + 0.1,
    las=1)
invisible(lapply(yr, function(x)
{
  bnd_inp<- boundary_product(inps)
  tmp<- dat[which(dat$year==x & is.na(dat$spawn_date) & dat$temperature_flow=="M"),]
  crvs<- spawning_survival_retention_curves(boundary_inputs = bnd_inp, 
                                            p_retained = tmp$p_retained)
  plot_boundary_curves(crvs, phi0_upper = 1, xaxis="n", xlabel="", ylabel="")
  if(!(x %in% c(1949, 1947, 1987, 1986)))
  {
    axis(1, at=seq(0,0.7, 0.1), labels=rep("",8), 
         tck=0.02, mgp=c(1.5,0.1,0))
  }
  if(x %in% c(1949, 1947, 1987, 1986))
  {
    axis(1, at=seq(0,0.7, 0.1), tck=0.02, mgp=c(1.5,0.1,0))
  }
  legend("topright", paste(x), bty="n")
}))
mtext(expression(paste("Spawning Probability (", gamma, ")")),
      1, outer=TRUE, padj=1)
mtext(expression(paste("Age-0 Survival ( ", phi[0], ")")),
      2, outer=TRUE, padj=1, las=0)


## LONG-TERM LAMBDA VS. P_RETAINED
ps<- seq(0.001, 1, 0.001)
ls_high<- sapply(ps, function(x)
{
  inpts<- inputs
  inpts$gamma<- 0.5
  inpts$p_retained<- x
  ea<- matrix_eigen_analysis(inpts)
  out<- ea$lambda1
  return(out)
})
ls_low<- sapply(ps, function(x)
{
  inpts<- inputs
  inpts$gamma<- 0.009
  inpts$p_retained<- x
  ea<- matrix_eigen_analysis(inpts)
  out<- ea$lambda1
  return(out)
})
plot(ps, ls_high, type="l", 
     #ylim=c(min(ls_high, ls_low), max(ls_high, ls_low)),
     xlab="Retention Probability",
     ylab="Long-Term Population Growth Rate")
points(ps, ls_low, type="l", col="gray")


#######################################################
#######################################################
#######################################################
tmp<- subset(dat, is.na(spawn_date))
tmp<- subset(tmp, temperature_flow %in% c("M", "M2", "M5"))
hist(tmp$p_retained, xlim=c(0,1), main="", xlab="Retention Proability")#, ylab="", yaxt='n')
mtext("Retention Probability", 1, padj=3)
mtext("Frequency", 2, las=0, padj=-3.5)
par(oma = c(2,2,1,0) + 0.1,
    mar = c(2,2,1,1) + 0.1)
#######################################################
#######################################################
#######################################################
params<- lapply(1:nrow(dat), function(i)
{
  ea<- readRDS(paste0("./output/scenario_eigen_analysis_g50_", dat$id[i], ".rds"))
  se<- sens_elas_table(ea, 100)
  # SENSITIVITY: TOP PARAMETERS
  sens<-se$Sensitivities[se$Sensitivities$Type %in% c("Parameter", "Both"),]
  if(any(sens$Parameter=="prod"))
  {
    indx<- which(sens$Parameter=="prod")
    sens<- sens[-indx,]
  }
  sens<- sens[order(sens$rank),]
  sens<- sens[sens$Sensitivity>=sens$Sensitivity[5],]
  sens_check<- all(c("phi0_MR", "p_retained", "sexratio", "gamma", 
                     "phi1") %in% sens$Parameter)
  sens_extra<- ifelse(length(setdiff(sens$Parameter, 
                                     c("phi0_MR", "p_retained", 
                                       "sexratio", "gamma", "phi1")))>0,
                      TRUE, FALSE)
  sens_dat<- data.frame(check=sens_check,
                        extra=sens_extra,
                        id=dat$id[i])
  # ELASTICITY: TOP PARAMETERS
  elas<-se$Elasticities[se$Elasticities$Type %in% c("Parameter", "Both"),]
  if(any(elas$Parameter=="prod"))
  {
    indx<- which(elas$Parameter=="prod")
    elas<- elas[-indx,]
  }
  elas<- elas[order(elas$rank),]
  elas<- elas[elas$Elasticity>=elas$Elasticity[5],]
  elas_check<- all(c("phi0_MR", "p_retained", "sexratio", "gamma", 
                     "phi1", paste0("phi", 2:7)) %in% elas$Parameter)
  elas_extra<- ifelse(length(setdiff(elas$Parameter, 
                                     c("phi0_MR", "p_retained", 
                                       "sexratio", "gamma", "phi1",
                                       paste0("phi", 2:7))))>0,
                      TRUE, FALSE)
  elas_extra_phi<- FALSE
  if(elas_extra)
  {
    elas_extra_phi<- all(setdiff(elas$Parameter, 
                                 c("phi0_MR", "p_retained", "sexratio", 
                                   "gamma", "phi1", paste0("phi", 2:7))) 
                         %in% paste0("phi", 8:59))
  }
  elas_dat<- data.frame(check=elas_check,
                        extra=elas_extra,
                        extra_phi=elas_extra_phi,
                        id=dat$id[i])
  return(list(Sensitivities=sens_dat, Elasticities=elas_dat))
})
sens<- do.call(rbind, lapply(params, "[[", 1))
elas<- do.call(rbind, lapply(params, "[[", 2))

all(sens$check==TRUE)
all(sens$extra==FALSE)
all(elas$check==TRUE)
indx<- which(elas$extra==TRUE)
all(elas$extra_phi[indx]==TRUE)

indx<- which(sens$check==FALSE)
indx<- sens$id[indx]
ea<- lapply(indx, function(i)
{
  tmp2<- readRDS(paste0("./output/scenario_eigen_analysis_g50_", i, ".rds"))
  se<- sens_elas_table(tmp2, 100)
  sens<-se$Sensitivities[se$Sensitivities$Type %in% c("Parameter", "Both"),]
  if(any(sens$Parameter=="prod"))
  {
    indx2<- which(sens$Parameter=="prod")
    sens<- sens[-indx2,]
  }
  sens<- sens[order(sens$rank),]
  sens$id<- i
  return(sens)
})

head(ea[[1]])
##p_retained>0.77 replaces gamma with phi2
### ALL BUT ONE OF THESE ARE FROM THE 1985 RUNS AND DON'T ALIGN WITH 
###OTHER DATA SO MAY BE IN ERROR 


params<- lapply(1:nrow(dat), function(i)
{
  ea<- readRDS(paste0("./output/scenario_eigen_analysis_g50_", dat$id[i], ".rds"))
  se<- sens_elas_table(ea, 100)
  # SENSITIVITY: TOP PARAMETERS
  sens<-se$Sensitivities[se$Sensitivities$Type %in% c("Parameter", "Both"),]
  if(any(sens$Parameter=="prod"))
  {
    indx<- which(sens$Parameter=="prod")
    sens<- sens[-indx,]
  }
  sens<- sens[order(sens$rank),]
  sens<- sens[sens$Sensitivity>=sens$Sensitivity[5],]
  sens_check1<- sens$Parameter[1]=="phi0_MR"
  sens_check2<- sens$Parameter[2]=="p_retained"
  sens_check3<- sens$Parameter[3]=="sexratio"
  sens_check4<- sens$Parameter[4]=="gamma"
  sens_check5<- sens$Parameter[5]=="phi1"
  sens_dat<- data.frame(check1=sens_check1,
                        check2=sens_check2,
                        check3=sens_check3,
                        check4=sens_check4,
                        check5=sens_check5,
                        id=dat$id[i])
  # ELASTICITY: TOP PARAMETERS
  elas<-se$Elasticities[se$Elasticities$Type %in% c("Parameter", "Both"),]
  if(any(elas$Parameter=="prod"))
  {
    indx<- which(elas$Parameter=="prod")
    elas<- elas[-indx,]
  }
  elas<- elas[order(elas$rank),]
  elas<- elas[elas$Elasticity>=elas$Elasticity[5],]
  elas_check<- elas$Elasticity[1]==elas$Elasticity[nrow(elas)]
  elas_dat<- data.frame(check=elas_check,
                        id=dat$id[i])
  return(list(Sensitivities=sens_dat, Elasticities=elas_dat))
})
sens<- do.call(rbind, lapply(params, "[[", 1))
elas<- do.call(rbind, lapply(params, "[[", 2))

all(sens$check1==TRUE)
indx<- which(sens$check1==FALSE)
indx<- sens$id[indx]
ea<- lapply(indx, function(i)
{
  tmp2<- readRDS(paste0("./output/scenario_eigen_analysis_g50_", i, ".rds"))
  se<- sens_elas_table(tmp2, 100)
  sens<-se$Sensitivities[se$Sensitivities$Type %in% c("Parameter", "Both"),]
  if(any(sens$Parameter=="prod"))
  {
    indx2<- which(sens$Parameter=="prod")
    sens<- sens[-indx2,]
  }
  sens<- sens[order(sens$rank),]
  sens$id<- i
  return(sens)
})


all(sens$check2==TRUE)
indx<- which(sens$check2==FALSE)
indx<- sens$id[indx]
indx<- setdiff(indx, 166)
ea<- lapply(indx, function(i)
{
  tmp2<- readRDS(paste0("./output/scenario_eigen_analysis_g50_", i, ".rds"))
  se<- sens_elas_table(tmp2, 100)
  sens<-se$Sensitivities[se$Sensitivities$Type %in% c("Parameter", "Both"),]
  if(any(sens$Parameter=="prod"))
  {
    indx2<- which(sens$Parameter=="prod")
    sens<- sens[-indx2,]
  }
  sens<- sens[order(sens$rank),]
  sens$id<- i
  return(sens)
})

head(ea[[1]])
all(sapply(1:length(ea), function(i)
{
  ea[[i]]$Parameter[2]
})=="sexratio")

all(sens$check3==TRUE)
indx3<- which(sens$check3==FALSE)
indx3<- sens$id[indx3]
indx3<- setdiff(indx3, indx)


all(sens$check4==TRUE)
indx<- which(sens$check4==FALSE)
indx<- sens$id[indx]
indx<- setdiff(indx, c(157,192,193,195,196,199))
ea<- lapply(indx, function(i)
{
  tmp2<- readRDS(paste0("./output/scenario_eigen_analysis_g50_", i, ".rds"))
  se<- sens_elas_table(tmp2, 100)
  sens<-se$Sensitivities[se$Sensitivities$Type %in% c("Parameter", "Both"),]
  if(any(sens$Parameter=="prod"))
  {
    indx2<- which(sens$Parameter=="prod")
    sens<- sens[-indx2,]
  }
  sens<- sens[order(sens$rank),]
  sens$id<- i
  return(sens)
})

head(ea[[1]])
all(sapply(1:length(ea), function(i)
{
  ea[[i]]$Parameter[2]
})=="sexratio")
all(sapply(1:length(ea), function(i)
{
  ea[[i]]$Parameter[3]
})=="gamma")

all(sens$check5==TRUE)
indx3<- which(sens$check5==FALSE)
indx3<- sens$id[indx3]
indx3<- setdiff(indx3, indx)
indx3<- setdiff(indx3, c(157,192,193,195,196,199))


all(elas$check==TRUE)

#######################################################
########################################################
#######################################################

# SENSITIVITY BARPLOT
tmp<- subset(dat, is.na(spawn_date) & temperature_flow=="M")
inps<- inputs
inps$gamma<-0.5
inps$p_retained<- median(tmp$p_retained)
ea<- matrix_eigen_analysis(inps)
tbl<- sens_elas_table(ea, 50)
sens<- tbl$Sensitivities[tbl$Sensitivities$Type %in% c("Parameter", "Both"),]
par(oma = c(3,6,1,0) + 0.1,
    mar = c(1,4,1,1) + 0.1)
barplot(sens$Sensitivity[7:2], 
        names.arg=c("Age-2 Survival", "Age-1 Survival", "Spawning Probability",
                    "Sex Ratio", "Retention Probability", "Survival Given Retention"),
        horiz=TRUE, xlab="", las=1)
mtext("Sensitivity", 1, padj=3)

elas<- tbl$Elasticities[tbl$Elasticities$Type %in% c("Parameter", "Both"),]
elas<-elas[-which(elas$Parameter=="prod"),]
elas<- elas[which(elas$Elasticity==elas$Elasticity[1]),]

par(oma = c(3,6,1,0) + 0.1,
    mar = c(1,4,1,1) + 0.1)
barplot(elas$Elasticity[1:5], 
        names.arg=c( "Survival","Spawning Probability",
                    "Sex Ratio", "Retention Probability", "Survival Given Retention"),
        horiz=TRUE, xlab="", las=1)
mtext("Elasticity", 1, padj=3)
